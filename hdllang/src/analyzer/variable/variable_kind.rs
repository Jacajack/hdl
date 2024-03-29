use std::collections::HashMap;

use crate::{
	analyzer::{
		module_implementation_scope::ExpressionEntryId, AdditionalContext, GlobalAnalyzerContext, LocalAnalyzerContext,
		SemanticError, SignalSensitivity, SignalSignedness, SignalType,
	},
	core::CompilerDiagnosticBuilder,
	parser::ast::SourceLocation,
	ProvidesCompilerDiagnostic,
};

use super::{
	module_implementation_scope::InternalVariableId, AlreadyCreated, BusType, BusWidth, GenericVariable,
	ModuleImplementationScope, ModuleInstance, Signal,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariableKind {
	Signal(Signal),
	Generic(GenericVariable),
	ModuleInstance(ModuleInstance),
}

impl VariableKind {
	/// only if needed
	pub fn add_name_to_clock(&mut self, id: InternalVariableId) {
		use VariableKind::*;
		match self {
			Signal(sig) => match &mut sig.sensitivity {
				SignalSensitivity::Clock(location, _) => {
					sig.sensitivity = SignalSensitivity::Clock(location.clone(), Some(id))
				},
				_ => (),
			},
			_ => (),
		}
	}
	pub fn evaluate_bus_width(
		&mut self,
		global_ctx: &GlobalAnalyzerContext,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
	) -> miette::Result<()> {
		use VariableKind::*;
		match self {
			Signal(sig) => {
				use SignalType::*;
				match &mut sig.signal_type {
					Bus(bus) => match &mut bus.width {
						Some(b) => {
							b.eval(global_ctx, scope, additional_ctx)?;
						},
						None => (),
					},
					_ => (),
				}
				for dim in &mut sig.dimensions {
					dim.eval(global_ctx, scope, additional_ctx)?;
				}
			},
			_ => unreachable!(),
		}
		Ok(())
	}
	pub fn remap_bus_widths(
		&mut self,
		global_ctx: &GlobalAnalyzerContext,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
		ids: &HashMap<ExpressionEntryId, ExpressionEntryId>,
	) -> miette::Result<()> {
		use VariableKind::*;
		match self {
			Signal(sig) => {
				use SignalType::*;
				match &mut sig.signal_type {
					Bus(bus) => match &mut bus.width {
						Some(b) => {
							b.remap(global_ctx, scope, additional_ctx, ids)?;
						},
						None => (),
					},
					_ => (),
				}
				for dim in &mut sig.dimensions {
					dim.remap(global_ctx, scope, additional_ctx, ids)?;
				}
			},
			_ => unreachable!(),
		}
		Ok(())
	}
	pub fn is_generic(&self) -> bool {
		use VariableKind::*;
		match self {
			Generic(_) => true,
			_ => false,
		}
	}
	pub fn is_array(&self) -> bool {
		use VariableKind::*;
		match self {
			Signal(sig) => sig.is_array(),
			_ => false,
		}
	}
	pub fn add_value(&mut self, value: BusWidth) -> Result<(), CompilerDiagnosticBuilder> {
		use VariableKind::*;
		match self {
			Generic(gen) => {
				match &mut gen.value {
					Some(_) => return Err(SemanticError::MultipleAssignment.to_diagnostic_builder()),
					None => gen.value = Some(value),
				}
				Ok(())
			},
			_ => panic!("Only generic variables can have values"),
		}
	}
	pub fn add_dimenstions(&mut self, dimensions: Vec<BusWidth>) {
		if dimensions.len() == 0 {
			return;
		}
		match self {
			VariableKind::Signal(signal) => signal.dimensions = dimensions,
			_ => panic!("Only signals can have dimensions"),
		}
	}
	pub fn to_signal(&self) -> Result<Signal, CompilerDiagnosticBuilder> {
		match self {
			VariableKind::Signal(signal) => Ok(signal.clone()),
			VariableKind::Generic(gen) => {
				if gen.value.is_none() && !gen.direction.is_input() {
					return Err(SemanticError::GenericUsedWithoutValue.to_diagnostic_builder());
				}
				match &gen.value {
					None => {
						log::debug!("Generic without value - interface parameter");
						let t = SignalType::Bus(BusType {
							width: Some(BusWidth::Evaluated(crate::core::NumericConstant::new_from_value(
								64.into(),
							))),
							signedness: gen.signedness.clone(),
							location: gen.location,
						});
						Ok(Signal {
							signal_type: t,
							dimensions: Vec::new(),
							sensitivity: SignalSensitivity::Const(gen.location),
							direction: gen.direction.clone(),
						})
					},
					Some(_) => {
						let t = SignalType::Bus(BusType {
							width: gen.width.clone(),
							signedness: gen.signedness.clone(),
							location: gen.location,
						});
						log::debug!("Generic with value - constant: {:?}", t);
						Ok(Signal {
							signal_type: t,
							dimensions: Vec::new(),
							sensitivity: SignalSensitivity::Const(gen.location),
							direction: gen.direction.clone(),
						})
					},
				}
			},
			VariableKind::ModuleInstance(_) => Err(SemanticError::ModuleInstantionUsedAsSignal.to_diagnostic_builder()),
		}
	}
	pub fn is_module_instance(&self) -> bool {
		use VariableKind::*;
		match self {
			ModuleInstance(_) => true,
			_ => false,
		}
	}
}

impl VariableKind {
	pub fn from_type_declarator(
		type_declarator: &crate::parser::ast::TypeDeclarator,
		current_scope: usize,
		mut already_created: AlreadyCreated,
		global_ctx: &crate::analyzer::GlobalAnalyzerContext,
		context: &mut Box<LocalAnalyzerContext>,
	) -> miette::Result<Self> {
		use crate::parser::ast::TypeSpecifier::*;
		match &type_declarator.specifier {
			Auto { location } => {
				already_created = crate::analyzer::analyze_qualifiers(
					&type_declarator.qualifiers,
					already_created,
					context,
					current_scope,
					&global_ctx.id_table,
				)?;
				if already_created.signedness != SignalSignedness::NoSignedness {
					return Ok(VariableKind::Signal(Signal {
						signal_type: SignalType::Bus(BusType {
							width: None,
							signedness: already_created.signedness,
							location: *location,
						}),
						dimensions: Vec::new(),
						sensitivity: already_created.sensitivity,
						direction: already_created.direction,
					}));
				}
				Ok(VariableKind::Signal(Signal {
					signal_type: SignalType::Auto(location.clone()),
					dimensions: Vec::new(),
					sensitivity: already_created.sensitivity,
					direction: already_created.direction,
				}))
			},
			Int { location } => {
				already_created = super::analyze_qualifiers(
					&type_declarator.qualifiers,
					already_created,
					context,
					current_scope,
					&global_ctx.id_table,
				)?;
				if already_created.signedness == SignalSignedness::NoSignedness {
					already_created.signedness = SignalSignedness::Signed(*location);
				}
				Ok(VariableKind::Generic(GenericVariable {
					value: None,
					width: None,
					is_wire: false,
					signedness: already_created.signedness,
					direction: already_created.direction,
					location: *location,
				}))
			},
			Wire { location } => {
				already_created = super::analyze_qualifiers(
					&type_declarator.qualifiers,
					already_created,
					context,
					current_scope,
					&global_ctx.id_table,
				)?;
				match already_created.signedness {
					SignalSignedness::NoSignedness => (),
					_ => {
						return Err(miette::Report::new(
							SemanticError::ContradictingSpecifier
								.to_diagnostic_builder()
								.label(*location, "Wire cannot be signed or unsigned")
								.build(),
						))
					},
				}
				Ok(VariableKind::Signal(Signal {
					signal_type: SignalType::Wire(*location),
					sensitivity: already_created.sensitivity,
					direction: already_created.direction,
					dimensions: Vec::new(),
				}))
			},
			Bool { location } => Ok(VariableKind::Generic(GenericVariable {
				value: None,
				width: Some(BusWidth::Evaluated(crate::core::NumericConstant::new_true())),
				is_wire: true,
				signedness: SignalSignedness::Unsigned(*location),
				direction: already_created.direction,
				location: *location,
			})),
			Bus(bus) => {
				already_created = super::analyze_qualifiers(
					&type_declarator.qualifiers,
					already_created,
					context,
					current_scope,
					&global_ctx.id_table,
				)?;
				if already_created.sensitivity.is_clock() {
					return Err(miette::Report::new(
						SemanticError::ContradictingSpecifier
							.to_diagnostic_builder()
							.label(bus.location, "Buses cannot be marked as clocks")
							.build(),
					));
				}
				let id = context.scope.add_expression(current_scope, *bus.width.clone());
				bus.width.evaluate_type(
					global_ctx,
					current_scope,
					context,
					Signal::new_empty(),
					false,
					bus.width.get_location(),
				)?;
				let width = if context.are_we_in_true_branch() {
					bus.width.eval(global_ctx, current_scope, context)?
				}
				else {
					None
				};

				let w = match &width {
					Some(val) => {
						if val.value <= num_bigint::BigInt::from(0) {
							log::debug!("Negative bus width: {:?}", val);
							return Err(miette::Report::new(
								SemanticError::NegativeBusWidth
									.to_diagnostic_builder()
									.label(bus.width.get_location(), "Array size must be positive")
									.build(),
							));
						}
						BusWidth::EvaluatedLocated(val.clone(), id)
					},
					None => BusWidth::Evaluable(id),
				};

				Ok(VariableKind::Signal(Signal {
					signal_type: SignalType::Bus(BusType {
						width: Some(w),
						signedness: already_created.signedness,
						location: bus.location,
					}),
					dimensions: Vec::new(),
					sensitivity: already_created.sensitivity,
					direction: already_created.direction,
				}))
			},
		}
	}
}
