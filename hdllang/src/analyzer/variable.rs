mod direction;
mod module_instance;
mod signal;
mod signal_sensitivity;
mod signal_signedness;
mod variable_kind;

use crate::{analyzer::module_implementation_scope::ExpressionEntryId, core::CommentTableKey, parser::ast::Res};
use core::panic;
pub use direction::*;
pub use module_instance::*;
pub use signal::*;
pub use signal_sensitivity::*;
pub use signal_signedness::*;
use std::{collections::HashMap, hash::Hash};
pub use variable_kind::*;

use hirn::design::{Evaluates, Expression, NumericConstant, SignalBuilder, SignalId};
use log::debug;
use num_bigint::BigInt;

use crate::{lexer::IdTableKey, SourceSpan};

use super::{module_implementation_scope::InternalVariableId, *};
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
	pub name: IdTableKey,
	/// location of the variable declaration
	pub location: SourceSpan,
	pub metadata_comment: Vec<CommentTableKey>,
	pub kind: VariableKind,
}
impl Variable {
	pub fn new(
		name: IdTableKey,
		location: SourceSpan,
		kind: VariableKind,
		metadata_comment: Vec<CommentTableKey>,
	) -> Self {
		Self {
			name,
			location,
			kind,
			metadata_comment,
		}
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
		global_ctx: &GlobalAnalyzerContext,
		scope_id: usize,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
		mut builder: SignalBuilder,
	) -> miette::Result<SignalId> {
		debug!(
			"registering variable {}\n {:?}",
			global_ctx.id_table.get_by_key(&self.name).unwrap(),
			self
		);
		if !self.metadata_comment.is_empty() {
			let mut comment = String::new();
			for com in &self.metadata_comment {
				comment.push_str(global_ctx.comment_table.get_by_key(&com).unwrap());
			}
			builder = builder.comment(comment.as_str());
		}
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
								let expr_ast = scope.get_expression(*location);
								expr_ast
									.expression
									.codegen(global_ctx, expr_ast.scope_id, scope, additional_ctx)?
							},
							Evaluable(location) => {
								log::debug!("Looking for expression at {:?}", location);
								let expr_ast = scope.get_expression(*location);
								expr_ast
									.expression
									.codegen(global_ctx, expr_ast.scope_id, scope, additional_ctx)?
							},
							WidthOf(location) => {
								let expr_ast = scope.get_expression(*location);
								let expr = expr_ast.expression.codegen(
									global_ctx,
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
							let expr = scope.get_expression(*location);
							let codegened =
								expr.expression
									.codegen(global_ctx, expr.scope_id, scope, additional_ctx)?;
							builder = builder.array(codegened).unwrap();
						},
						Evaluable(location) => {
							let expr = scope.get_expression(*location).expression.codegen(
								global_ctx,
								scope_id,
								scope,
								additional_ctx,
							)?;
							builder = builder.array(expr).unwrap();
						},
						WidthOf(location) => {
							let mut expr = scope.get_expression(*location).expression.codegen(
								global_ctx,
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
							let expr_ast = scope.get_expression(*location);
							expr_ast
								.expression
								.codegen(global_ctx, expr_ast.scope_id, scope, additional_ctx)?
						},
						Evaluable(location) => {
							let expr_ast = scope.get_expression(*location);
							expr_ast
								.expression
								.codegen(global_ctx, expr_ast.scope_id, scope, additional_ctx)?
						},
						WidthOf(location) => {
							let expr_ast = scope.get_expression(*location);
							let expr =
								expr_ast
									.expression
									.codegen(global_ctx, expr_ast.scope_id, scope, additional_ctx)?;
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
	EvaluatedLocated(crate::core::NumericConstant, ExpressionEntryId), // in non generic modules
	Evaluable(ExpressionEntryId),            // in generic modules
	WidthOf(ExpressionEntryId),              // in generic modules
}
impl PartialEq for BusWidth {
	fn eq(&self, other: &Self) -> bool {
		use BusWidth::*;
		match (self, other) {
			(Evaluated(l0), Evaluated(r0)) => l0 == r0,
			(EvaluatedLocated(l0, _), EvaluatedLocated(r0, _)) => l0 == r0,
			(Evaluated(l0), EvaluatedLocated(r0, _)) => l0 == r0,
			(EvaluatedLocated(l0, _), Evaluated(r0)) => l0 == r0,
			_ => true,
		}
	}
}
impl BusWidth {
	pub fn get_location(&self) -> Option<ExpressionEntryId> {
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
	pub fn eval(
		&mut self,
		global_ctx: &GlobalAnalyzerContext,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
	) -> miette::Result<()> {
		use BusWidth::*;
		match self {
			Evaluated(_) => (),
			EvaluatedLocated(..) => (),
			Evaluable(location) => {
				let expr_ast = scope.get_expression(*location);
				let expr = {
					let val = expr_ast
						.expression
						.eval_with_hirn(global_ctx, expr_ast.scope_id, scope, additional_ctx);
					match val {
						Ok(expr) => {
							let ctx = hirn::design::EvalContext::without_assumptions(global_ctx.design.clone());
							let val = expr.eval(&ctx).unwrap(); // FIXME handle errors
							Some(crate::core::NumericConstant::from_hirn_numeric_constant(val))
						},
						Err(res) => match res {
							Res::Err(err) => return Err(err),
							Res::GenericValue => None,
						},
					}
				};
				match expr {
					Some(expr) => {
						*self = EvaluatedLocated(expr, *location);
					},
					None => (),
				}
			},
			WidthOf(_) => (),
		}
		Ok(())
	}
	pub fn remap(
		&mut self,
		global_ctx: &GlobalAnalyzerContext,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
		ids: &HashMap<ExpressionEntryId, ExpressionEntryId>,
	) -> miette::Result<()> {
		use BusWidth::*;
		match self {
			Evaluated(_) => (),
			EvaluatedLocated(_, loc) => {
				loc.clone_from(ids.get(&loc).unwrap());
			},
			Evaluable(location) => {
				let expr_ast = scope.get_expression(*location);
				location.clone_from(ids.get(&location).unwrap());
				let expr = {
					let val = expr_ast
						.expression
						.eval_with_hirn(global_ctx, expr_ast.scope_id, scope, additional_ctx);
					match val {
						Ok(expr) => {
							let ctx = hirn::design::EvalContext::without_assumptions(global_ctx.design.clone());
							let val = expr.eval(&ctx).unwrap(); // FIXME handle errors
							Some(crate::core::NumericConstant::from_hirn_numeric_constant(val))
						},
						Err(res) => match res {
							Res::Err(err) => return Err(err),
							Res::GenericValue => None,
						},
					}
				};
				match expr {
					Some(expr) => {
						*self = EvaluatedLocated(expr, *location);
					},
					None => (),
				}
			},
			WidthOf(location) => location.clone_from(ids.get(&location).unwrap()),
		}
		Ok(())
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
