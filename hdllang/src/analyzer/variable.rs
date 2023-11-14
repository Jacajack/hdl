mod direction;
mod module_instance;
mod signal;
mod signal_sensitivity;
mod signal_signedness;
mod variable_kind;

use core::panic;
pub use direction::*;
pub use module_instance::*;
pub use signal::*;
pub use signal_sensitivity::*;
pub use signal_signedness::*;
use std::hash::Hash;
pub use variable_kind::*;

use hirn::design::{Expression, NumericConstant, SignalBuilder, SignalId};
use log::debug;
use num_bigint::BigInt;

use crate::{
	core::id_table::{self},
	lexer::IdTableKey, SourceSpan,
};

use super::{module_implementation_scope::InternalVariableId, *};
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
	pub name: IdTableKey,
	/// location of the variable declaration
	pub location: SourceSpan,
	pub kind: VariableKind,
}
impl Variable {
	pub fn new(name: IdTableKey, location: SourceSpan, kind: VariableKind) -> Self {
		Self { name, location, kind }
	}
	pub fn is_clock(&self) -> bool {
		use SignalSensitivity::*;
		use VariableKind::*;
		match &self.kind {
			Signal(signal) => match &signal.sensitivity {
				Clock(..) => true,
				_ => false,
			},
			Generic(_) => false,
			ModuleInstance(_) => false,
		}
	}
	pub fn get_clock_name(&self) -> InternalVariableId {
		use VariableKind::*;
		match &self.kind {
			Signal(signal) => signal.get_clock_name(),
			Generic(_) => panic!("This variable is not a signal"),
			ModuleInstance(_) => panic!("This variable is not a signal"),
		}
	}
	/// To be done by VariableDefined struct as it has internal variable id
	/// and can insert it into scope automatically
	pub fn register(
		&self,
		nc_table: &crate::lexer::NumericConstantTable,
		id_table: &id_table::IdTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
		mut builder: SignalBuilder,
	) -> miette::Result<SignalId> {
		debug!(
			"registering variable {}\n {:?}",
			id_table.get_by_key(&self.name).unwrap(),
			self
		);
		let id: SignalId;
		use VariableKind::*;
		match &self.kind {
			Signal(signal) => {
				use SignalSensitivity::*;
				match &signal.sensitivity {
					Async(_) => builder = builder.asynchronous(),
					Comb(list, _) => {
						for edge in &list.list {
							let id = scope.get_api_id_by_internal_id(edge.clock_signal).unwrap();
							builder = builder.comb(id, edge.on_rising);
						}
					},
					Sync(list, _) => {
						for edge in &list.list {
							let id = scope.get_api_id_by_internal_id(edge.clock_signal).unwrap();
							builder = builder.sync(id, edge.on_rising);
						}
					},
					Clock(..) => builder = builder.clock(),
					Const(_) => builder = builder.constant(),
					NoSensitivity => unreachable!("No sensitivity should not be possible"),
				}
				match &signal.signal_type {
					SignalType::Bus(bus) => {
						use BusWidth::*;
						debug!("Width is {:?}", bus.width.clone().unwrap());
						let width = match &bus.width.clone().unwrap() {
							Evaluated(value) => {
								Expression::Constant(hirn::design::NumericConstant::new_signed(value.clone().value))
							},
							EvaluatedLocated(_, location) => {
								let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
								expr_ast.expression.codegen(
									nc_table,
									id_table,
									expr_ast.scope_id,
									scope,
									additional_ctx,
								)?
							},
							Evaluable(location) => {
								log::debug!("Looking for expression at {:?}", location);
								let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
								expr_ast.expression.codegen(
									nc_table,
									id_table,
									expr_ast.scope_id,
									scope,
									additional_ctx,
								)?
							},
							WidthOf(location) => {
								let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
								let expr = expr_ast.expression.codegen(
									nc_table,
									id_table,
									expr_ast.scope_id,
									scope,
									additional_ctx,
								)?;
								log::debug!("Width of is {:?}", expr);
								hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Width(Box::new(expr)))
							}, 
						};
						debug!("Width is {:?}", width);
						match bus.signedness {
							SignalSignedness::Signed(_) => builder = builder.signed(width),
							SignalSignedness::Unsigned(_) => builder = builder.unsigned(width),
							SignalSignedness::NoSignedness => unreachable!(),
						}
					},
					SignalType::Wire(_) => builder = builder.wire(),
					_ => unreachable!("Only bus and wire types are allowed"),
				}
				for dimension in &signal.dimensions {
					use BusWidth::*;
					match &dimension {
						Evaluated(value) => {
							builder = builder
								.array(Expression::from(NumericConstant::new_unsigned(value.clone().value)))
								.unwrap()
						},
						EvaluatedLocated(_, location) => {
							let expr = scope.evaluated_expressions.get(location).unwrap();
							let codegened =
								expr.expression
									.codegen(nc_table, id_table, expr.scope_id, scope, additional_ctx)?;
							builder = builder.array(codegened).unwrap();
						},
						Evaluable(location) => {
							let expr = scope.evaluated_expressions.get(location).unwrap().expression.codegen(
								nc_table,
								id_table,
								scope_id,
								scope,
								additional_ctx,
							)?;
							builder = builder.array(expr).unwrap();
						},
						WidthOf(location) => {
							let mut expr = scope.evaluated_expressions.get(location).unwrap().expression.codegen(
								nc_table,
								id_table,
								scope_id,
								scope,
								additional_ctx,
							)?;
							expr = hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Width(Box::new(expr)));
							builder = builder.array(expr).unwrap();
						},
					}
				}
				id = builder.build().unwrap();
			},
			Generic(generic) => {
				use BusWidth::*;
				let width = match &generic.width {
					Some(width) => match width {
						Evaluated(value) => {
							Expression::Constant(hirn::design::NumericConstant::new_signed(value.clone().value))
						},
						EvaluatedLocated(_, location) => {
							let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
							expr_ast
								.expression
								.codegen(nc_table, id_table, expr_ast.scope_id, scope, additional_ctx)?
						},
						Evaluable(location) => {
							let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
							expr_ast
								.expression
								.codegen(nc_table, id_table, expr_ast.scope_id, scope, additional_ctx)?
						},
						WidthOf(location) => {
							let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
							let expr = expr_ast
								.expression
								.codegen(nc_table, id_table, expr_ast.scope_id, scope, additional_ctx)?;
							hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Width(Box::new(expr)))
						},
					},
					None => Expression::Constant(hirn::design::NumericConstant::new_signed(64.into())),
				};
				if generic.is_wire {
					builder = builder.wire();
				}
				else {
					match generic.signedness {
						SignalSignedness::Signed(_) => builder = builder.signed(width),
						SignalSignedness::Unsigned(_) => builder = builder.unsigned(width),
						SignalSignedness::NoSignedness => unreachable!(),
					}
				}
				builder = builder.generic();
				id = builder.build().unwrap();
			},
			ModuleInstance(_) => unreachable!("Module instantion should not be possible here"),
		}
		Ok(id)
	}
}

#[derive(Debug, Clone, Hash, Eq)]
pub enum BusWidth {
	Evaluated(crate::core::NumericConstant), // in non generic modules
	EvaluatedLocated(crate::core::NumericConstant, SourceSpan), // in non generic modules
	Evaluable(SourceSpan),                   // in generic modules
	WidthOf(SourceSpan),                     // in generic modules
}
impl PartialEq for BusWidth {
	fn eq(&self, other: &Self) -> bool {
		use BusWidth::*;
		match (self, other) {
			(Evaluated(l0), Evaluated(r0)) => l0 == r0,
			(EvaluatedLocated(l0, _), EvaluatedLocated(r0, _)) => l0 == r0,
			(Evaluated(l0), EvaluatedLocated(r0, _)) => l0 == r0,
			(EvaluatedLocated(l0, _), Evaluated(r0)) => l0 == r0,
			_=> true,
		}
	}
}
impl BusWidth {
	pub fn get_location(&self) -> Option<SourceSpan> {
		use BusWidth::*;
		match self {
			EvaluatedLocated(_, location) => Some(*location),
			Evaluated(_) => None,
			Evaluable(location) => Some(*location),
			WidthOf(location) => Some(*location),
		}
	}
	pub fn get_value(&self) -> Option<BigInt> {
		use BusWidth::*;
		match self {
			Evaluated(value) => Some(value.clone().value),
			EvaluatedLocated(value, _) => Some(value.clone().value),
			Evaluable(_) => None,
			WidthOf(_) => None,
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenericVariable {
	pub value: Option<BusWidth>,
	pub width: Option<BusWidth>,
	pub is_wire: bool,
	pub signedness: SignalSignedness,
	pub direction: Direction,
	pub location: SourceSpan,
}
impl GenericVariable {
	pub fn is_direction_specified(&self) -> bool {
		use Direction::*;
		match &self.direction {
			Input(_) => true,
			Output(_) => true,
			Tristate(_) => true,
			None => false,
		}
	}
}
