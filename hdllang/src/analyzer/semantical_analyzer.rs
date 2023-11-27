use super::{GlobalAnalyzerContext, LocalAnalyzerContext, ModuleImplementationScope, SemanticError};
use crate::core::CompilerDiagnosticBuilder;
use crate::parser::ast::ModuleImplementation;
use crate::{core::IdTableKey, ProvidesCompilerDiagnostic};
use crate::{CompilerDiagnostic, CompilerError};
use hirn::elab::{ElabMessageKind, ElabMessageSeverity, ElabToplevelAssumptions, Elaborator, FullElaborator};
use log::*;
use std::io::Write;
use std::{collections::HashMap, sync::Arc};
pub struct SemanticalAnalyzer<'a> {
	ctx: GlobalAnalyzerContext<'a>,
	modules_implemented: &'a HashMap<IdTableKey, &'a ModuleImplementation>,
	passes: Vec<
		for<'b> fn(
			&mut GlobalAnalyzerContext<'a>,
			&mut Box<LocalAnalyzerContext>,
			&ModuleImplementation,
		) -> miette::Result<()>,
	>,
}
impl<'a> SemanticalAnalyzer<'a> {
	pub fn new(
		ctx: GlobalAnalyzerContext<'a>,
		modules_implemented: &'a HashMap<IdTableKey, &ModuleImplementation>,
	) -> Self {
		Self {
			ctx,
			modules_implemented,
			passes: Vec::new(),
		}
	}
	pub fn buffer(&self) -> crate::core::DiagnosticBuffer {
		self.ctx.diagnostic_buffer.clone()
	}
	pub fn semantical_analysis(&mut self) -> miette::Result<()> {
		self.passes.push(first_pass);
		self.passes.push(second_pass);
		for module in self.modules_implemented.values() {
			let scope = match self.ctx.modules_declared.get(&module.id) {
				Some(m) => m.scope.clone(),
				None => {
					return Err(miette::Report::new(
						SemanticError::ModuleNotDeclared
							.to_diagnostic_builder()
							.label(
								module.location,
								format!(
									"Declaration of {:?} module cannot be found",
									self.ctx.id_table.get_by_key(&module.id).unwrap()
								)
								.as_str(),
							)
							.build(),
					))
				},
			};
			let mut local_ctx = LocalAnalyzerContext::new(module.id, scope);
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}
		}
		Ok(())
	}
	pub fn compile_and_elaborate(&mut self, output: &mut dyn Write) -> miette::Result<()> {
		self.passes.push(first_pass);
		self.passes.push(second_pass);
		self.passes.push(codegen_pass);

		let mut scopes: HashMap<hirn::design::ModuleId, ModuleImplementationScope> = HashMap::new();

		let generic_modules = self.ctx.generic_modules.clone();
		for module in generic_modules.values() {
			let scope = match self.ctx.modules_declared.get(&module.id) {
				Some(m) => m.scope.clone(),
				None => {
					return Err(miette::Report::new(
						SemanticError::ModuleNotDeclared
							.to_diagnostic_builder()
							.label(
								module.location,
								format!(
									"Declaration of {:?} module cannot be found",
									self.ctx.id_table.get_by_key(&module.id).unwrap()
								)
								.as_str(),
							)
							.build(),
					))
				},
			};
			let mut local_ctx = LocalAnalyzerContext::new(module.id, scope);
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}

			let module_id = self
				.ctx
				.modules_declared
				.get_mut(&local_ctx.module_id())
				.unwrap()
				.handle
				.id();

			scopes.insert(module_id, local_ctx.scope);
		}

		for module in self.modules_implemented.values() {
			let scope = match self.ctx.modules_declared.get(&module.id) {
				Some(m) => m.scope.clone(),
				None => {
					return Err(miette::Report::new(
						SemanticError::ModuleNotDeclared
							.to_diagnostic_builder()
							.label(
								module.location,
								format!(
									"Declaration of {:?} module cannot be found",
									self.ctx.id_table.get_by_key(&module.id).unwrap()
								)
								.as_str(),
							)
							.build(),
					))
				},
			};
			let mut local_ctx = LocalAnalyzerContext::new(module.id, scope);
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}

			let module_id = self
				.ctx
				.modules_declared
				.get_mut(&local_ctx.module_id())
				.unwrap()
				.handle
				.id();
			scopes.insert(module_id, local_ctx.scope.clone());
		}
		for module in self.modules_implemented.values() {
			let module_id = self.ctx.modules_declared.get_mut(&module.id).unwrap().handle.id();
			let mut elab = FullElaborator::new(self.ctx.design.clone());
			let elab_result = elab.elaborate(
				module_id,
				Arc::new(ElabToplevelAssumptions::new(self.ctx.design.clone())),
			);
			match elab_result {
				Ok(elab_report) => {
					for msg in elab_report.messages() {
						let id = msg.module_id();
						log::debug!("Module id: {:?}", id);
						log::debug!("Scopes: {:?}", scopes.keys());
						match msg.default_severity() {
							ElabMessageSeverity::Error => self.ctx.diagnostic_buffer.push_error(to_report(
								&scopes.get(&msg.module_id()).unwrap(),
								module.location,
								CompilerDiagnosticBuilder::new_error(msg.to_string().as_str()),
								msg.kind(),
							)),
							ElabMessageSeverity::Warning => self.ctx.diagnostic_buffer.push_diagnostic(to_report(
								&scopes.get(&msg.module_id()).unwrap(),
								module.location,
								CompilerDiagnosticBuilder::new_warning(msg.to_string().as_str()),
								msg.kind(),
							)),
							ElabMessageSeverity::Info => self.ctx.diagnostic_buffer.push_diagnostic(to_report(
								&scopes.get(&msg.module_id()).unwrap(),
								module.location,
								CompilerDiagnosticBuilder::new_info(msg.to_string().as_str()),
								msg.kind(),
							)),
						}
					}
				},
				Err(err) => {
					return Err(miette::Report::new(
						CompilerError::ElaborationError(err)
							.to_diagnostic_builder()
							.label(
								module.location,
								"During elaboration of module this module critical error occured",
							)
							.build(),
					));
				},
			};
			if self.buffer().contains_errors() {
				continue;
			}
			let mut output_string = String::new();
			let mut sv_codegen = hirn::codegen::sv::SVCodegen::new(self.ctx.design.clone(), &mut output_string);
			use hirn::codegen::Codegen;
			sv_codegen.emit_module(module_id).unwrap();
			write!(output, "{}", output_string).unwrap();
		}
		// if elab did not catch any errors, then we can proceed to codegen generic modules
		for module in self.ctx.generic_modules.values() {
			let module_id = self.ctx.modules_declared.get_mut(&module.id).unwrap().handle.id();
			let mut output_string = String::new();
			let mut sv_codegen = hirn::codegen::sv::SVCodegen::new(self.ctx.design.clone(), &mut output_string);
			use hirn::codegen::Codegen;
			sv_codegen.emit_module(module_id).unwrap();
			write!(output, "{}", output_string).unwrap();
		}
		Ok(())
	}

	pub fn compile(&mut self, output: &mut dyn Write) -> miette::Result<()> {
		self.passes.push(first_pass);
		self.passes.push(second_pass);
		self.passes.push(codegen_pass);
		let generic_modules = self.ctx.generic_modules.clone();
		for module in generic_modules.values() {
			let scope = match self.ctx.modules_declared.get(&module.id) {
				Some(m) => m.scope.clone(),
				None => {
					return Err(miette::Report::new(
						SemanticError::ModuleNotDeclared
							.to_diagnostic_builder()
							.label(
								module.location,
								format!(
									"Declaration of {:?} module cannot be found",
									self.ctx.id_table.get_by_key(&module.id).unwrap()
								)
								.as_str(),
							)
							.build(),
					))
				},
			};
			let mut local_ctx = LocalAnalyzerContext::new(module.id, scope);
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}

			let module_id = self
				.ctx
				.modules_declared
				.get_mut(&local_ctx.module_id())
				.unwrap()
				.handle
				.id();

			let mut output_string = String::new();
			let mut sv_codegen = hirn::codegen::sv::SVCodegen::new(self.ctx.design.clone(), &mut output_string);
			use hirn::codegen::Codegen;
			sv_codegen.emit_module(module_id).unwrap();
			write!(output, "{}", output_string).unwrap();
		}

		for module in self.modules_implemented.values() {
			let scope = match self.ctx.modules_declared.get(&module.id) {
				Some(m) => m.scope.clone(),
				None => {
					return Err(miette::Report::new(
						SemanticError::ModuleNotDeclared
							.to_diagnostic_builder()
							.label(
								module.location,
								format!(
									"Declaration of {:?} module cannot be found",
									self.ctx.id_table.get_by_key(&module.id).unwrap()
								)
								.as_str(),
							)
							.build(),
					))
				},
			};
			let mut local_ctx = LocalAnalyzerContext::new(module.id, scope);
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}

			let module_id = self
				.ctx
				.modules_declared
				.get_mut(&local_ctx.module_id())
				.unwrap()
				.handle
				.id();

			let mut output_string = String::new();
			let mut sv_codegen = hirn::codegen::sv::SVCodegen::new(self.ctx.design.clone(), &mut output_string);
			use hirn::codegen::Codegen;
			sv_codegen.emit_module(module_id).unwrap();
			write!(output, "{}", output_string).unwrap();
		}
		Ok(())
	}
}
/// This pass collects all variables
pub fn first_pass(
	ctx: &mut GlobalAnalyzerContext,
	local_ctx: &mut Box<LocalAnalyzerContext>,
	module: &ModuleImplementation,
) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running initial pass");
	module.first_pass(ctx, local_ctx)?;
	debug!("Initial pass done");
	Ok(())
}
/// This pass checks if all variables have specified sensitivity and width if needed
fn second_pass(
	ctx: &mut GlobalAnalyzerContext,
	local_ctx: &mut Box<LocalAnalyzerContext>,
	module: &ModuleImplementation,
) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running second pass");
	module.second_pass(ctx, local_ctx)?;
	debug!("Second pass done");
	Ok(())
}
/// This pass invokes HIRN API and creates proper module
fn codegen_pass(
	ctx: &mut GlobalAnalyzerContext,
	local_ctx: &mut Box<LocalAnalyzerContext>,
	module: &ModuleImplementation,
) -> miette::Result<()> {
	// first, generic codegen pass needs to be conducted
	// FIXME when intermidiate signal insertion is implemented
	// then other statements and blocks can be codegened
	module.codegen_pass(ctx, local_ctx)?;
	Ok(())
}

fn to_report(
	scope: &ModuleImplementationScope,
	module_location: crate::SourceSpan,
	report: CompilerDiagnosticBuilder,
	elab_report_kind: &ElabMessageKind,
) -> CompilerDiagnostic {
	use ElabMessageKind::*;
	match elab_report_kind {
		WidthMismatch {
			lhs,
			lhs_width,
			rhs_width,
		} => {
			let lhs_location = scope.get_variable_location(lhs.signal());
			report
				.label(lhs_location, format!("Width of this signal is {}", lhs_width).as_str())
				.help(format!("Cannot assign {} bits to {} bit signal", rhs_width, lhs_width).as_str())
				.build()
		},

		CombLoop {signals} => {
			// FIXME this report is terrible
			// Reporting signal references should be reworked, because it's very
			// broken for arrays.
			report
				.help(format!("The combinational loop may involve some of these signals: {:?}", signals).as_str())
				.build()
		}

		SignalNotDriven { signal, elab } => {
			let variable_location = scope.get_variable_location(signal.signal());
			let mask = elab.undriven_summary();
			use hirn::elab::SignalMaskSummary::*;
			match mask {
				Empty => unreachable!(),
				Full => report.label(variable_location, "This signal does not have any drivers"),
				Single(bit) => report.label(
					variable_location,
					format!("Bit {} of this signal is not driven", bit).as_str(),
				),
				Ranges(ranges) => {
					let mut label_msg = from_range_to_string(ranges);
					label_msg.push_str("of this signal are not driven");
					report.label(variable_location, label_msg.as_str())
				},
			}
			.build()
		},
		SignalNotDrivenAndUsed { signal, elab } => {
			let variable_location = scope.get_variable_location(signal.signal());
			let mask = elab.undriven_summary();
			use hirn::elab::SignalMaskSummary::*;
			match mask {
				Empty => unreachable!(),
				Full => report.label(
					variable_location,
					"This signal does not have any drivers but is being used",
				),
				Single(bit) => report.label(
					variable_location,
					format!("Bit {} of this signal is not driven but used", bit).as_str(),
				),
				Ranges(ranges) => {
					let mut label_msg = from_range_to_string(ranges);
					label_msg.push_str("of this signal are not driven but used");
					report.label(variable_location, label_msg.as_str())
				},
			}
			.build()
		},
		SignalUnused { signal, elab } => {
			let variable_location = scope.get_variable_location(signal.signal());
			let mask = elab.unread_summary();
			use hirn::elab::SignalMaskSummary::*;
			match mask {
				Empty => unreachable!(),
				Full => report.label(variable_location, "This signal is never being used"),
				Single(bit) => report.label(
					variable_location,
					format!("Bit {} of this signal is never being used", bit).as_str(),
				),
				Ranges(ranges) => {
					let mut label_msg = from_range_to_string(ranges);
					label_msg.push_str("of this signal are never being used");
					report.label(variable_location, label_msg.as_str())
				},
			}
			.build()
		},
		SignalConflict { signal, elab } => {
			let variable_location = scope.get_variable_location(signal.signal());
			let mask = elab.conflict_summary();
			use hirn::elab::SignalMaskSummary::*;
			match mask {
				Empty => unreachable!(),
				Full => report.label(variable_location, "This signal has multiple drivers"),
				Single(bit) => report.label(
					variable_location,
					format!("Bit {} of this signal is driven more than once", bit).as_str(),
				),
				Ranges(ranges) => {
					let mut label_msg = from_range_to_string(ranges);
					label_msg.push_str("are driven more than once");
					report.label(variable_location, label_msg.as_str())
				},
			}
			.build()
		},
		Notice(msg) => report.label(module_location, msg.as_str()).build(),
		_ => report
			.label(module_location, "Other elaboration warning/error occured")
			.build(),
	}
}

fn from_range_to_string(ranges: Vec<(u32, u32)>) -> String {
	assert!(!ranges.is_empty());

	let first_range = ranges.first().unwrap();
	let mut label_msg = if first_range.0 == first_range.1 {
		format!("Bit {} ", first_range.0)
	}
	else {
		format!("Bits {}-{} ", first_range.0, first_range.1)
	};
	for i in 1..ranges.len() {
		let (begin, end) = ranges[i];
		if begin == end {
			label_msg.push_str(format!("and {} ", begin).as_str());
		}
		else {
			label_msg.push_str(format!("and {}-{} ", begin, end).as_str());
		}
	}
	label_msg
}
