use super::{GlobalAnalyzerContext, LocalAnalyzerContext, SemanticError};
use crate::parser::ast::ModuleImplementation;
use crate::{core::IdTableKey, ProvidesCompilerDiagnostic};
use hirn::elab::{ElabMessageSeverity, ElabToplevelAssumptions, Elaborator, FullElaborator};
use log::{debug, error, info, warn};
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
				.get_mut(&local_ctx.module_id)
				.unwrap()
				.handle
				.id();

			// FIXME use Miette here
			let mut elab = FullElaborator::new(self.ctx.design.clone());
			let elab_result = elab.elaborate(module_id, Arc::new(ElabToplevelAssumptions::default()));
			match elab_result {
				Ok(elab_report) => {
					for msg in elab_report.messages() {
						match msg.severity() {
							ElabMessageSeverity::Error => {
								error!("elab: {}", msg);
							},
							ElabMessageSeverity::Warning => {
								warn!("elab: {}", msg);
							},
							ElabMessageSeverity::Info => {
								info!("elab: {}", msg);
							},
						}
					}
				},

				Err(err) => {
					panic!("Fatal elab error: {}", err);
				},
			};

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
			let mut output_string = String::new();
			let mut sv_codegen = hirn::codegen::sv::SVCodegen::new(self.ctx.design.clone(), &mut output_string);
			use hirn::codegen::Codegen;
			sv_codegen
				.emit_module(
					self.ctx
						.modules_declared
						.get_mut(&local_ctx.module_id)
						.unwrap()
						.handle
						.id(),
				)
				.unwrap();
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
pub fn second_pass(
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
pub fn codegen_pass(
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
