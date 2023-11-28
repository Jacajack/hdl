mod binary_expression;
mod conditional_expression;
mod identifier;
mod match_expression;
mod number;
mod parenthesized;
mod postfix_with_args;
mod postfix_with_id;
mod postfix_with_index;
mod postfix_with_range;
mod pretty_printable;
mod ternary_expression;
mod tuple;
mod unary_cast_expression;
mod unary_operator_expression;

use crate::analyzer::module_implementation_scope::{EvaluatedEntry, InternalVariableId};
use crate::analyzer::{
	AdditionalContext, AlreadyCreated, BusWidth, EdgeSensitivity, GlobalAnalyzerContext, LocalAnalyzerContext,
	ModuleImplementationScope, ModuleInstanceKind, SemanticError, Signal, SignalSensitivity, SignalSignedness,
	SignalType, VariableKind,
};
use crate::core::{CompilerDiagnosticBuilder, NumericConstant};
use crate::lexer::IdTableKey;
use crate::parser::ast::{
	opcodes::*, MatchExpressionAntecendent, MatchExpressionStatement, RangeExpression, SourceLocation, TypeName,
};
use crate::{ProvidesCompilerDiagnostic, SourceSpan};
pub use binary_expression::BinaryExpression;
pub use conditional_expression::ConditionalExpression;
use hirn::design::SignalSlice;
pub use identifier::Identifier;
use log::debug;
pub use match_expression::MatchExpression;
use num_bigint::{BigInt, Sign};
use num_traits::{ToPrimitive, Zero};
pub use number::Number;
pub use parenthesized::ParenthesizedExpression;
pub use postfix_with_args::PostfixWithArgs;
pub use postfix_with_id::PostfixWithId;
pub use postfix_with_index::PostfixWithIndex;
pub use postfix_with_range::PostfixWithRange;
use std::cmp::max;
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::vec;
pub use ternary_expression::TernaryExpression;
pub use tuple::Tuple;
pub use unary_cast_expression::UnaryCastExpression;
pub use unary_operator_expression::UnaryOperatorExpression;
#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
	Number(Number),
	Identifier(Identifier), // takes in part in two-sided type deduction  as a rhs
	ParenthesizedExpression(ParenthesizedExpression),
	MatchExpression(MatchExpression),
	ConditionalExpression(ConditionalExpression),
	Tuple(Tuple),
	TernaryExpression(TernaryExpression),
	PostfixWithIndex(PostfixWithIndex), // takes in part in two-sided type deduction as a rhs
	PostfixWithRange(PostfixWithRange), // takes in part in two-sided type deduction as a rhs
	PostfixWithArgs(PostfixWithArgs),   // takes in part in two-sided type deduction as a rhs
	PostfixWithId(PostfixWithId),       // takes in part in two-sided type deduction as a rhs
	UnaryOperatorExpression(UnaryOperatorExpression),
	UnaryCastExpression(UnaryCastExpression),
	BinaryExpression(BinaryExpression),
}

impl Debug for Expression {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::Expression::*;
		match &self {
			Number(number) => write!(fmt, "{:?}", number.key),
			Identifier(_) => write!(fmt, "foo"),
			ParenthesizedExpression(expr) => write!(fmt, "({:?})", *expr.expression),
			BinaryExpression(binop) => {
				write!(fmt, "({:?} {:?} {:?})", binop.lhs, binop.code, binop.rhs)
			},
			TernaryExpression(teranary) => {
				write!(
					fmt,
					"({:?} ? {:?} : {:?})",
					teranary.condition, teranary.true_branch, teranary.false_branch
				)
			},
			UnaryOperatorExpression(unary) => write!(fmt, "{:?}{:?}", unary.code, unary.expression),
			PostfixWithId(postfix) => write!(fmt, "({:?}.{:?})", postfix.expression, postfix.id),
			PostfixWithIndex(postfix) => write!(fmt, "({:?}[{:?}])", postfix.expression, postfix.index),
			PostfixWithRange(postfix) => write!(fmt, "({:?}{:?})", postfix.expression, postfix.range),
			PostfixWithArgs(postfix) => {
				write!(fmt, "({:?}(", postfix.id)?;
				for arg in postfix.argument_list.iter() {
					write!(fmt, "{:?}, ", arg)?;
				}
				write!(fmt, "))")
			},
			UnaryCastExpression(unary) => write!(fmt, "(({:?}){:?})", unary.type_name, unary.expression),
			Tuple(tuple) => {
				write!(fmt, "{{")?;
				for expr in tuple.expressions.iter() {
					write!(fmt, "{:?},", expr)?;
				}
				write!(fmt, "}}")
			},
			MatchExpression(match_expr) => {
				writeln!(fmt, "match({:?}){{", match_expr.value)?;
				for s in match_expr.statements.iter() {
					writeln!(fmt, "{:?},", s)?;
				}
				write!(fmt, "}}")
			},
			ConditionalExpression(cond) => {
				writeln!(fmt, "conditional{{")?;
				for s in cond.statements.iter() {
					writeln!(fmt, "{:?},", s)?;
				}
				write!(fmt, "}}")
			},
		}
	}
}
impl SourceLocation for Expression {
	fn get_location(&self) -> SourceSpan {
		use self::Expression::*;
		match self {
			Number(number) => number.location,
			Identifier(ident) => ident.location,
			ParenthesizedExpression(expr) => expr.location,
			MatchExpression(match_expr) => match_expr.location,
			ConditionalExpression(cond) => cond.location,
			Tuple(tuple) => tuple.location,
			TernaryExpression(ternary) => ternary.location,
			PostfixWithIndex(postfix) => postfix.location,
			PostfixWithRange(postfix) => postfix.location,
			PostfixWithArgs(postfix) => postfix.location,
			PostfixWithId(postfix) => postfix.location,
			UnaryOperatorExpression(unary) => unary.location,
			UnaryCastExpression(unary) => unary.location,
			BinaryExpression(binop) => binop.location,
		}
	}
}

impl Expression {
	pub fn transform<T>(&mut self, f: &dyn Fn(&mut Expression) -> Result<(), T>) -> Result<(), T> {
		log::debug!("Transforming expression {:?}", self);
		use self::Expression::*;
		match self {
			Number(_) => (),
			Identifier(_) => (),
			ParenthesizedExpression(expr) => {
				expr.expression.transform(f)?;
			},
			MatchExpression(expr) => {
				expr.value.transform(f)?;
				for statement in &mut expr.statements {
					use MatchExpressionAntecendent::*;
					match &mut statement.antecedent {
						Expression {
							expressions,
							location: _,
						} => {
							for expr in expressions {
								expr.transform(f)?;
							}
						},
						Default { .. } => (),
					}
					statement.expression.transform(f)?;
				}
			},
			ConditionalExpression(expr) => {
				for statement in &mut expr.statements {
					use MatchExpressionAntecendent::*;
					match &mut statement.antecedent {
						Expression {
							expressions,
							location: _,
						} => {
							for expr in expressions {
								expr.transform(f)?;
							}
						},
						Default { .. } => (),
					}
					statement.expression.transform(f)?;
				}
			},
			Tuple(_) => unreachable!(),
			TernaryExpression(tern) => {
				tern.condition.transform(f)?;
				tern.true_branch.transform(f)?;
				tern.false_branch.transform(f)?;
			},
			PostfixWithIndex(postfix) => {
				postfix.expression.transform(f)?;
				postfix.index.transform(f)?;
			},
			PostfixWithRange(expr) => {
				expr.expression.transform(f)?;
				expr.range.lhs.transform(f)?;
				expr.range.rhs.transform(f)?;
			},
			PostfixWithArgs(expr) => {
				for arg in expr.argument_list.iter_mut() {
					arg.transform(f)?;
				}
			},
			PostfixWithId(_) => (),
			UnaryOperatorExpression(un) => {
				un.expression.transform(f)?;
			},
			UnaryCastExpression(expr) => {
				expr.expression.transform(f)?;
			},
			BinaryExpression(binop) => {
				binop.lhs.transform(f)?;
				binop.rhs.transform(f)?;
			},
		};
		f(self)?;
		Ok(())
	}
	/// deprecated, not used
	pub fn get_name_sync_or_comb(&self) -> miette::Result<IdTableKey> {
		use self::Expression::*;
		match self {
			Identifier(ident) => Ok(ident.id),
			UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					LogicalNot => unary.expression.get_name_sync_or_comb(),
					_ => {
						return Err(miette::Report::new(
							SemanticError::ForbiddenExpressionInSyncOrComb
								.to_diagnostic_builder()
								.label(
									unary.location,
									"This expression is not allowed in sync or comb qualifier",
								)
								.build(),
						))
					},
				}
			},
			_ => {
				return Err(miette::Report::new(
					SemanticError::ForbiddenExpressionInSyncOrComb
						.to_diagnostic_builder()
						.label(
							self.get_location(),
							"This expression is not allowed in sync or comb qualifier",
						)
						.build(),
				))
			},
		}
	}
	pub fn create_edge_sensitivity(
		&self,
		current_scope: usize,
		scope: &ModuleImplementationScope,
		id_table: &crate::lexer::IdTable,
		list_location: SourceSpan,
	) -> miette::Result<EdgeSensitivity> {
		use self::Expression::*;
		match self {
			Identifier(id) => {
				let var = match scope.get_variable(current_scope, &id.id) {
					None => {
						return Err(miette::Report::new(
							SemanticError::VariableNotDeclared
								.to_diagnostic_builder()
								.label(
									id.location,
									format!(
										"This variable {:?} is not declared",
										id_table.get_by_key(&id.id).unwrap()
									)
									.as_str(),
								)
								.build(),
						))
					},
					Some(var) => var,
				};
				if !var.is_clock() {
					return Err(miette::Report::new(
						SemanticError::NotClockSignalInSync
							.to_diagnostic_builder()
							.label(list_location, "This sync list contains non-clock signals")
							.label(
								id.location,
								format!(
									"This variable {:?} is not a clock signal",
									id_table.get_by_key(&id.id).unwrap()
								)
								.as_str(),
							)
							.build(),
					));
				}
				Ok(EdgeSensitivity {
					clock_signal: var.id,
					on_rising: true,
					location: id.location,
				})
			},
			UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					LogicalNot => unary
						.expression
						.create_edge_sensitivity(current_scope, scope, id_table, list_location)
						.map(|mut edge| {
							edge.on_rising = !edge.on_rising;
							edge
						}),
					_ => {
						return Err(miette::Report::new(
							SemanticError::ForbiddenExpressionInSyncOrComb
								.to_diagnostic_builder()
								.label(
									unary.location,
									"This expression is not allowed in sync or comb qualifier",
								)
								.build(),
						))
					},
				}
			},
			_ => {
				return Err(miette::Report::new(
					SemanticError::ForbiddenExpressionInSyncOrComb
						.to_diagnostic_builder()
						.label(
							self.get_location(),
							"This expression is not allowed in sync or comb qualifier",
						)
						.build(),
				))
			},
		}
	}
	pub fn assign(
		&self,
		value: BusWidth,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		scope_id: usize,
	) -> Result<(), CompilerDiagnosticBuilder> {
		use Expression::*;
		match self {
			Identifier(id) => {
				if local_ctx.scope.is_declared(scope_id, &id.id).is_none() {
					return Err(SemanticError::VariableNotDeclared.to_diagnostic_builder().label(
						id.location,
						"This variable is not defined in this scope, so it cannot be assigned to here",
					));
				}
				let mut var = local_ctx.scope.get_variable(scope_id, &id.id).unwrap().clone();
				var.var
					.kind
					.add_value(value)
					.map_err(|err| err.label(self.get_location(), "Cannot assign value to this variable"))?;
				local_ctx.scope.redeclare_variable(var);
				Ok(())
			},
			_ => unreachable!(),
		}
	}
	pub fn evaluate(
		// 1) jest, bo jest 2) jest, tu masz wartośc 3) nie może być bo nie wiadomo
		&self,
		nc_table: &crate::lexer::NumericConstantTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
	) -> miette::Result<Option<NumericConstant>> {
		match self {
			Expression::Number(nc_key) => {
				let constant = nc_table.get_by_key(&nc_key.key).unwrap();
				Ok(Some(constant.clone()))
			},
			Expression::Identifier(id) => {
				let var = match scope.get_variable(scope_id, &id.id) {
					Some(var) => var,
					None => {
						log::debug!("Scope id is {:?}", scope_id);
						return Err(miette::Report::new(
							SemanticError::VariableNotDeclared
								.to_diagnostic_builder()
								.label(id.location, "This variable is not defined in this scope")
								.build(),
						));
					},
				};
				use crate::analyzer::VariableKind::*;
				match &var.var.kind {
					Signal(_) => Err(miette::Report::new(
						SemanticError::NonGenericTypeVariableInExpression
							.to_diagnostic_builder()
							.label(
								id.location,
								"This variable is used in expression but its value its not known at compile time",
							)
							.build(),
					)),
					Generic(generic) => match &generic.value {
						Some(val) => match val {
							BusWidth::Evaluated(nc) => Ok(Some(nc.clone())),
							BusWidth::EvaluatedLocated(nc, _) => Ok(Some(nc.clone())),
							_ => Ok(None),
						},
						None => {
							if let crate::analyzer::Direction::Input(_) = &generic.direction {
								return Ok(None);
							}
							return Err(miette::Report::new(
							SemanticError::NonGenericTypeVariableInExpression
								.to_diagnostic_builder()
								.label(
									id.location,
									"This variable is used in expression but its value its not known at compile time",
								)
								.build(),
						));
						},
					},
					ModuleInstance(_) => unreachable!(),
				}
			},
			Expression::ParenthesizedExpression(expr) => expr.expression.evaluate(nc_table, scope_id, scope),
			Expression::MatchExpression(expr) => report_not_allowed_expression(expr.location, "match"),
			Expression::ConditionalExpression(expr) => report_not_allowed_expression(expr.location, "conditional"),
			Expression::Tuple(_) => unreachable!(),
			Expression::TernaryExpression(tern) => {
				let val = tern.condition.evaluate(nc_table, scope_id, scope)?;
				if val.is_none() {
					return Ok(None);
				}
				Ok(
					if tern.condition.evaluate(nc_table, scope_id, scope)?.unwrap().value != BigInt::from(0) {
						tern.true_branch.evaluate(nc_table, scope_id, scope)?
					}
					else {
						tern.false_branch.evaluate(nc_table, scope_id, scope)?
					},
				)
			},
			Expression::PostfixWithIndex(expr) => report_not_allowed_expression(expr.location, "index"),
			Expression::PostfixWithRange(expr) => report_not_allowed_expression(expr.location, "range"), // nie bedzie
			Expression::PostfixWithArgs(_) => todo!(),
			Expression::PostfixWithId(expr) => report_not_allowed_expression(expr.location, "postfix with id"), // nie bedzie
			Expression::UnaryOperatorExpression(unary) => {
				use crate::core::numeric_constant::*;
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					LogicalNot => {
						let expr = unary.expression.evaluate(nc_table, scope_id, scope)?;
						if expr.is_none() {
							return Ok(None);
						}
						Ok(
							if unary.expression.evaluate(nc_table, scope_id, scope)?.unwrap().value == BigInt::from(0) {
								Some(NumericConstant::new(
									BigInt::from(1),
									None,
									None,
									Some(NumericConstantBase::Boolean),
								))
							}
							else {
								Some(NumericConstant::new(
									BigInt::from(0),
									None,
									None,
									Some(NumericConstantBase::Boolean),
								))
							},
						)
					},
					BitwiseNot => Ok(NumericConstant::new_from_unary(
						unary.expression.evaluate(nc_table, scope_id, scope)?,
						|e| !e,
					)),
					Minus => {
						let other = unary.expression.evaluate(nc_table, scope_id, scope)?;
						// FIXME if let Some(false) = other.signed {
						// FIXME 	// report an error
						// FIXME }
						Ok(NumericConstant::new_from_unary(other, |e| -e))
					},
					Plus => Ok(NumericConstant::new_from_unary(
						unary.expression.evaluate(nc_table, scope_id, scope)?,
						|e| e * (-1),
					)),
				}
			},
			Expression::UnaryCastExpression(expr) => report_not_allowed_expression(expr.location, "unary cast"), // nie bedzie
			Expression::BinaryExpression(binop) => {
				let lhs = binop.lhs.evaluate(nc_table, scope_id, scope)?;
				let rhs = binop.rhs.evaluate(nc_table, scope_id, scope)?;
				if lhs.is_none() || rhs.is_none() {
					return Ok(None);
				}
				use crate::core::numeric_constant::*;
				use crate::parser::ast::BinaryOpcode::*;
				match binop.code {
					Multiplication => Ok(NumericConstant::new_from_binary(lhs, rhs, |e1, e2| e1 * e2)),
					Division => {
						if rhs.clone().unwrap().value.is_zero() {
							return Err(miette::Report::new(
								SemanticError::DivisionByZero
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Division by zero is not allowed")
									.build(),
							));
						}

						Ok(NumericConstant::new_from_binary(lhs, rhs, |e1, e2| e1 / e2))
					},
					Addition => Ok(NumericConstant::new_from_binary(lhs, rhs, |e1, e2| e1 + e2)),
					Subtraction => Ok(NumericConstant::new_from_binary(lhs, rhs, |e1, e2| e1 - e2)),
					Modulo => {
						if rhs.is_some() {
							if rhs.clone().unwrap().value.is_zero() {
								return Err(miette::Report::new(
									SemanticError::DivisionByZero
										.to_diagnostic_builder()
										.label(binop.rhs.get_location(), "It is not allowed to modulo by zero")
										.build(),
								));
							}
						}
						Ok(NumericConstant::new_from_binary(lhs, rhs, |e1, e2| e1 / e2))
					},
					Equal => Ok(if lhs.unwrap().value == rhs.unwrap().value {
						Some(NumericConstant::from_u64(
							1,
							Some(1),
							Some(false),
							Some(NumericConstantBase::Boolean),
						))
					}
					else {
						Some(NumericConstant::from_u64(
							0,
							Some(1),
							Some(false),
							Some(NumericConstantBase::Boolean),
						))
					}),
					NotEqual => Ok(if lhs.unwrap().value != rhs.unwrap().value {
						Some(NumericConstant::from_u64(
							1,
							Some(1),
							Some(false),
							Some(NumericConstantBase::Boolean),
						))
					}
					else {
						Some(NumericConstant::from_u64(
							0,
							Some(1),
							Some(false),
							Some(NumericConstantBase::Boolean),
						))
					}),
					LShift => {
						if rhs.clone().unwrap().value.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						}
						else {
							let mut i = BigInt::from(0);
							let mut lhs = lhs.clone().unwrap();
							while i < rhs.clone().unwrap().value {
								lhs.value = lhs.value << 1;
								i += BigInt::from(1);
							}
							Ok(Some(lhs))
						}
					},
					RShift => {
						if rhs.clone().unwrap().value.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						}
						else {
							let mut i = BigInt::from(0);
							let mut lhs = lhs.clone().unwrap();
							while i < rhs.clone().unwrap().value {
								lhs.value = lhs.value >> 1;
								i += BigInt::from(1);
							}
							Ok(Some(lhs))
						}
					},
					BitwiseAnd => Ok(NumericConstant::new_from_binary(lhs, rhs, |e1, e2| e1 & e2)),
					BitwiseOr => Ok(NumericConstant::new_from_binary(lhs, rhs, |e1, e2| e1 | e2)),
					BitwiseXor => Ok(NumericConstant::new_from_binary(lhs, rhs, |e1, e2| e1 ^ e2)),
					Less => Ok(if lhs.unwrap().value < rhs.unwrap().value {
						Some(NumericConstant::new_true())
					}
					else {
						Some(NumericConstant::new_false())
					}),
					Greater => Ok(if lhs.unwrap().value > rhs.unwrap().value {
						Some(NumericConstant::new_true())
					}
					else {
						Some(NumericConstant::new_false())
					}),
					LessEqual => Ok(if lhs.unwrap().value <= rhs.unwrap().value {
						Some(NumericConstant::new_true())
					}
					else {
						Some(NumericConstant::new_false())
					}),
					GreaterEqual => Ok(if lhs.unwrap().value >= rhs.unwrap().value {
						Some(NumericConstant::new_true())
					}
					else {
						Some(NumericConstant::new_false())
					}),
					LogicalAnd => Ok(
						if lhs.unwrap().value != BigInt::from(0) && rhs.unwrap().value != BigInt::from(0) {
							Some(NumericConstant::new_true())
						}
						else {
							Some(NumericConstant::new_false())
						},
					),
					LogicalOr => Ok(
						if lhs.unwrap().value != BigInt::from(0) || rhs.unwrap().value != BigInt::from(0) {
							Some(NumericConstant::new_true())
						}
						else {
							Some(NumericConstant::new_false())
						},
					),
				}
			},
		}
	}
	// deprecated
	pub fn get_slice(
		&self,
		nc_table: &crate::lexer::NumericConstantTable,
		id_table: &crate::lexer::IdTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
	) -> miette::Result<SignalSlice> {
		use self::Expression::*;
		match self {
			Identifier(id) => {
				let signal_id = scope.get_api_id(scope_id, &id.id).unwrap();
				Ok(SignalSlice {
					signal: signal_id,
					indices: vec![],
				})
			},
			ParenthesizedExpression(expr) => {
				expr.expression
					.get_slice(nc_table, id_table, scope_id, scope, additional_ctx)
			},
			PostfixWithIndex(ind) => {
				let index_expr = ind.index.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				let mut slice = ind
					.expression
					.get_slice(nc_table, id_table, scope_id, scope, additional_ctx)?;
				slice.indices.push(index_expr);
				Ok(slice)
			},
			_ => {
				debug!("expr {:?}", self);
				unreachable!()
			},
		}
	}
	pub fn is_signedness_specified(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		local_ctx: &LocalAnalyzerContext,
		current_scope: usize,
	) -> bool {
		use Expression::*;
		match self {
			Number(nc) => {
				let constant = global_ctx.nc_table.get_value(&nc.key);
				match constant.signed {
					Some(val) => val,
					None => false,
				}
			},
			BinaryExpression(binop) => {
				binop.lhs.is_signedness_specified(global_ctx, local_ctx, current_scope)
					|| binop.rhs.is_signedness_specified(global_ctx, local_ctx, current_scope)
			},
			UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					Minus => true,
					Plus => true,
					_ => unary
						.expression
						.is_signedness_specified(global_ctx, local_ctx, current_scope),
				}
			},
			Identifier(_) => todo!(),
			ParenthesizedExpression(expr) => {
				expr.expression
					.is_signedness_specified(global_ctx, local_ctx, current_scope)
			},
			MatchExpression(_) => todo!(),
			ConditionalExpression(_) => todo!(),
			Tuple(_) => unreachable!(),
			TernaryExpression(tern) => {
				tern.true_branch
					.is_signedness_specified(global_ctx, local_ctx, current_scope)
					|| tern
						.false_branch
						.is_signedness_specified(global_ctx, local_ctx, current_scope)
			},
			PostfixWithIndex(_) => true, // FIXME
			PostfixWithRange(_) => true,
			PostfixWithArgs(_) => todo!(),
			PostfixWithId(_) => todo!(),
			UnaryCastExpression(_) => todo!(),
		}
	}
	pub fn is_lvalue(&self) -> bool {
		use Expression::*;
		match self {
			Identifier(_) => true,
			PostfixWithIndex(_) => true,
			PostfixWithRange(_) => true,
			ParenthesizedExpression(expr) => expr.expression.is_lvalue(),
			_ => false,
		}
	}
	pub fn is_width_specified(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		local_ctx: &LocalAnalyzerContext,
		current_scope: usize,
	) -> bool {
		use Expression::*;
		match self {
			Number(nc) => {
				let constant = global_ctx.nc_table.get_value(&nc.key);
				match constant.width {
					Some(_) => true,
					None => false,
				}
			},
			Identifier(id) => {
				let var = local_ctx.scope.get_variable(current_scope, &id.id).unwrap();
				match &var.var.kind {
					VariableKind::Signal(sig) => {
						use SignalType::*;
						match &sig.signal_type {
							Bus(bus) => bus.width.is_some(),
							Wire(_) => true,
							Auto(_) => false,
						}
					},
					_ => false,
				}
			},
			ParenthesizedExpression(expr) => expr.expression.is_width_specified(global_ctx, local_ctx, current_scope),
			MatchExpression(m) => {
				m.get_default()
					.unwrap()
					.expression
					.is_width_specified(global_ctx, local_ctx, current_scope) //FIXME
			},
			ConditionalExpression(_) => todo!(),
			Tuple(_) => todo!(),
			TernaryExpression(tern) => {
				tern.true_branch
					.is_width_specified(global_ctx, local_ctx, current_scope)
					|| tern
						.false_branch
						.is_width_specified(global_ctx, local_ctx, current_scope)
			},
			PostfixWithIndex(_) => true,
			PostfixWithRange(_) => true,
			PostfixWithArgs(_) => todo!(),
			PostfixWithId(_) => false,
			UnaryOperatorExpression(_) => true,
			UnaryCastExpression(_) => true,
			BinaryExpression(_) => true,
		}
	}
	pub fn codegen(
		&self,
		nc_table: &crate::core::NumericConstantTable,
		id_table: &crate::lexer::IdTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
	) -> miette::Result<hirn::design::Expression> {
		use self::Expression::*;
		match self {
			Number(num) => {
				if let Some(ncs) = additional_ctx {
					log::debug!("Location is {:?}", self.get_location());
					log::debug!("ncs.nc_widths is {:?}", ncs.ncs_to_be_exted);
					if let Some(loc) = ncs.ncs_to_be_exted.get(&self.get_location()) {
						debug!("Found a constant to be extended {:?}", loc);
						let constant = nc_table.get_by_key(&num.key).unwrap();
						let value: BigInt = constant.value.clone();
						let width_loc = scope.get_expression(*loc);
						let width = width_loc.expression.codegen(
							nc_table,
							id_table,
							width_loc.scope_id,
							scope,
							additional_ctx,
						)?;
						let signed = if let Some(nc) = ncs.nc_widths.get(&self.get_location()) {
							log::debug!("We will read sign!");
							match nc.signed {
								Some(s) => s,
								None => true,
							}
						}
						else {
							match constant.signed {
								Some(s) => s,
								None => true,
							}
						};
						log::debug!("Sign is {:?}", signed);
						let constant = hirn::design::Expression::Constant(
							hirn::design::NumericConstant::from_bigint(
								constant.value.clone(),
								if signed {
									hirn::design::SignalSignedness::Signed
								}
								else {
									hirn::design::SignalSignedness::Unsigned
								},
								max(value.bits() + if signed { 1 } else { 0 }, 1),
							)
							.unwrap(),
						);
						if signed {
							return Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::SignExtend {
								expr: Box::new(constant),
								width: Box::new(width),
							}));
						}
						else {
							return Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::ZeroExtend {
								expr: Box::new(constant),
								width: Box::new(width),
							}));
						}
					}
					if let Some(nc) = ncs.nc_widths.get(&num.location) {
						return Ok(hirn::design::Expression::Constant(
							hirn::design::NumericConstant::from_bigint(
								nc.value.clone(),
								match nc.signed {
									Some(val) => {
										if val {
											hirn::design::SignalSignedness::Signed
										}
										else {
											hirn::design::SignalSignedness::Unsigned
										}
									},
									None => hirn::design::SignalSignedness::Signed,
								},
								nc.width.unwrap_or(64).into(),
							)
							.unwrap(),
						));
					}
					//if let Some(loc) = ncs.ncs_to_be_exted.get(&self.get_location()){
					//	debug!("Found a constant to be extended {:?}", loc);
					//	let constant = nc_table.get_by_key(&num.key).unwrap();
					//	let value:BigInt = constant.value.clone();
					//	let width_loc = scope.evaluated_expressions.get(loc).unwrap().clone();
					//	let width = width_loc.expression.codegen(nc_table, id_table, width_loc.scope_id, scope, additional_ctx)?;
					//	let signed = match constant.signed {
					//		Some(s) => s,
					//		None => true,
					//	};
					//	let constant = hirn::design::Expression::Constant(
					//		hirn::design::NumericConstant::from_bigint(
					//			constant.value.clone(),
					//			if signed {
					//				hirn::design::SignalSignedness::Signed
					//			}
					//			else {
					//				hirn::design::SignalSignedness::Unsigned
					//			},
					//			(value.bits() + BigInt::from(1)).to_u64().unwrap()
					//		)
					//		.unwrap(),
					//	);
					//	return Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::SignExtend {
					//		expr: Box::new(constant),
					//		width: Box::new(width),
					//	}));
					//}
				}
				let constant = nc_table.get_by_key(&num.key).unwrap();
				let signed = match constant.signed {
					Some(s) => s,
					None => true,
				};
				let w = match constant.width.is_some() {
					true => constant.width.unwrap(),
					_ => 64,
				};
				debug!("Width is {:?}", w);
				Ok(hirn::design::Expression::Constant(
					hirn::design::NumericConstant::from_bigint(
						constant.value.clone(),
						if signed {
							hirn::design::SignalSignedness::Signed
						}
						else {
							hirn::design::SignalSignedness::Unsigned
						},
						w.into(),
					)
					.unwrap(),
				))
			},
			Identifier(id) => {
				let signal_id = scope.get_api_id(scope_id, &id.id).unwrap();
				Ok(signal_id.into())
			},
			ParenthesizedExpression(expr) => {
				expr.expression
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)
			},
			MatchExpression(match_expr) => {
				let def = match_expr.get_default().unwrap();
				let mut builder = hirn::design::Expression::new_conditional(def.expression.codegen(
					nc_table,
					id_table,
					scope_id,
					scope,
					additional_ctx,
				)?);
				for stmt in &match_expr.statements {
					let cond = match_expr
						.value
						.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
					match &stmt.antecedent {
						MatchExpressionAntecendent::Expression {
							expressions,
							location: _,
						} => {
							let val = stmt
								.expression
								.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
							for expr in expressions {
								builder = builder.branch(
									hirn::design::Expression::Binary(hirn::design::BinaryExpression {
										lhs: Box::new(cond.clone()),
										op: hirn::design::BinaryOp::Equal,
										rhs: Box::new(expr.codegen(
											nc_table,
											id_table,
											scope_id,
											scope,
											additional_ctx,
										)?),
									}),
									val.clone(),
								);
							}
						},
						MatchExpressionAntecendent::Default { location: _ } => (),
					};
				}
				Ok(builder.build())
			},
			ConditionalExpression(cond) => {
				let def = cond.get_default().unwrap();
				let mut builder = hirn::design::Expression::new_conditional(def.expression.codegen(
					nc_table,
					id_table,
					scope_id,
					scope,
					additional_ctx,
				)?);
				for stmt in &cond.statements {
					match &stmt.antecedent {
						MatchExpressionAntecendent::Expression {
							expressions,
							location: _,
						} => {
							let val = stmt
								.expression
								.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
							for expr in expressions {
								builder = builder.branch(
									expr.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?,
									val.clone(),
								);
							}
						},
						MatchExpressionAntecendent::Default { location: _ } => (),
					};
				}
				Ok(builder.build())
			},
			Tuple(_) => unreachable!(),
			TernaryExpression(ternary) => {
				let cond = ternary
					.condition
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				let true_branch = ternary
					.true_branch
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				let false_branch = ternary
					.false_branch
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				let builder = hirn::design::Expression::new_conditional(false_branch);
				Ok(builder.branch(cond, true_branch).build())
			},
			PostfixWithIndex(ind) => {
				if let Some(ctx) = additional_ctx {
					let is_array = ctx.array_or_bus.get(&self.get_location()).unwrap().clone();
					let index = ind.index.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
					if is_array {
						let mut expr = ind
							.expression
							.get_slice(nc_table, id_table, scope_id, scope, additional_ctx)?;
						expr.indices.push(index);
						return Ok(expr.into());
					}
					else {
						let expr = ind
							.expression
							.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
						return Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::BitSelect {
							expr: Box::new(expr),
							index: Box::new(index),
						}));
					}
				}
				unreachable!()
			},
			PostfixWithRange(range) => {
				let expr = range
					.expression
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				let mut msb = range
					.range
					.rhs
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				let lsb = range
					.range
					.lhs
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				use RangeOpcode::*;
				match range.range.code {
					Colon => (),
					PlusColon => msb = msb + lsb.clone(),
					ColonLessThan => {
						msb = msb
							- hirn::design::Expression::Constant(hirn::design::NumericConstant::new_signed(1.into()))
					},
				}
				Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::BusSelect {
					expr: Box::new(expr),
					msb: Box::new(msb),
					lsb: Box::new(lsb),
				}))
			},
			PostfixWithArgs(function) => {
				let func_name = id_table.get_value(&function.id);
				match func_name.as_str() {
					"trunc" => {
						let expr = function.argument_list.first().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						let width = scope.widths.get(&function.location).unwrap().clone(); //FIXME
						log::debug!("Width is {:?}", width);
						if let Some(loc) = width.get_location() {
							let op = hirn::design::BuiltinOp::BusSelect {
								expr: Box::new(expr),
								msb: Box::new(
									scope.get_expression(loc).expression.codegen(
										nc_table,
										id_table,
										scope_id,
										scope,
										additional_ctx,
									)? - hirn::design::Expression::Constant(hirn::design::NumericConstant::new_signed(
										BigInt::from(1),
									)),
								),
								lsb: Box::new(hirn::design::Expression::Constant(
									hirn::design::NumericConstant::new_signed(BigInt::from(0)),
								)),
							};
							return Ok(hirn::design::Expression::Builtin(op));
						}
						if let Some(val) = &width.get_value() {
							let op = hirn::design::BuiltinOp::BusSelect {
								expr: Box::new(expr),
								msb: Box::new(hirn::design::Expression::Constant(
									hirn::design::NumericConstant::new_unsigned(val.clone() - 1),
								)),
								lsb: Box::new(hirn::design::Expression::Constant(
									hirn::design::NumericConstant::new_unsigned(BigInt::from(0)),
								)),
							};
							return Ok(hirn::design::Expression::Builtin(op));
						}
						unreachable!()
					},
					"zeros" => {
						let expr = hirn::design::NumericConstant::from_bigint(
							0.into(),
							hirn::design::SignalSignedness::Unsigned,
							1,
						)
						.unwrap(); // FIXME
						let count = function.argument_list.first().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						debug!("Count is {:?}", count);
						debug!("Expr is {:?}", expr);
						Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Replicate {
							expr: Box::new(expr.into()),
							count: Box::new(count),
						}))
					},
					"ones" => {
						let expr = hirn::design::NumericConstant::from_bigint(
							1.into(),
							hirn::design::SignalSignedness::Unsigned,
							1,
						)
						.unwrap(); // FIXME
						let count = function.argument_list.first().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Replicate {
							expr: Box::new(expr.into()),
							count: Box::new(count),
						}))
					},
					"zext" | "ext" | "sext" => {
						let expr = function.argument_list.first().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						let width = scope.widths.get(&function.location).unwrap().clone(); //FIXME
						log::debug!("Width is {:?}", width);
						if let Some(loc) = width.get_location() {
							let op = match func_name.as_str() {
								"zext" => hirn::design::BuiltinOp::ZeroExtend {
									expr: Box::new(expr),
									width: Box::new(scope.get_expression(loc).expression.codegen(
										nc_table,
										id_table,
										scope_id,
										scope,
										additional_ctx,
									)?),
								},
								"ext" => hirn::design::BuiltinOp::SignExtend {
									expr: Box::new(expr),
									width: Box::new(scope.get_expression(loc).expression.codegen(
										nc_table,
										id_table,
										scope_id,
										scope,
										additional_ctx,
									)?),
								},
								"sext" => hirn::design::BuiltinOp::SignExtend {
									expr: Box::new(expr),
									width: Box::new(scope.get_expression(loc).expression.codegen(
										nc_table,
										id_table,
										scope_id,
										scope,
										additional_ctx,
									)?),
								},
								_ => unreachable!(),
							};
							return Ok(hirn::design::Expression::Builtin(op));
						}
						if let Some(val) = &width.get_value() {
							let op = match func_name.as_str() {
								"zext" => hirn::design::BuiltinOp::ZeroExtend {
									expr: Box::new(expr),
									width: Box::new(hirn::design::Expression::Constant(
										hirn::design::NumericConstant::new_unsigned(val.clone()),
									)),
								},
								"ext" => hirn::design::BuiltinOp::SignExtend {
									expr: Box::new(expr),
									width: Box::new(hirn::design::Expression::Constant(
										hirn::design::NumericConstant::new_unsigned(val.clone()),
									)),
								},
								"sext" => hirn::design::BuiltinOp::SignExtend {
									expr: Box::new(expr),
									width: Box::new(hirn::design::Expression::Constant(
										hirn::design::NumericConstant::new_unsigned(val.clone()),
									)),
								},
								_ => unreachable!(),
							};
							return Ok(hirn::design::Expression::Builtin(op));
						}
						unreachable!()
					},
					"join" => {
						let mut exprs = Vec::new();
						for expr in &function.argument_list {
							exprs.push(expr.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?);
						}
						Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Join(exprs)))
					},
					"rep" => {
						let expr = function.argument_list.first().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						let count = function.argument_list.last().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Replicate {
							expr: Box::new(expr),
							count: Box::new(count),
						}))
					},
					"fold_or" => {
						let expr = function.argument_list.first().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Unary(hirn::design::UnaryExpression {
							op: hirn::design::UnaryOp::ReductionOr,
							operand: Box::new(expr),
						}))
					},
					"fold_xor" => {
						let expr = function.argument_list.first().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Unary(hirn::design::UnaryExpression {
							op: hirn::design::UnaryOp::ReductionXor,
							operand: Box::new(expr),
						}))
					},
					"fold_and" => {
						let expr = function.argument_list.first().unwrap().codegen(
							nc_table,
							id_table,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Unary(hirn::design::UnaryExpression {
							op: hirn::design::UnaryOp::ReductionAnd,
							operand: Box::new(expr),
						}))
					},
					_ => unreachable!(),
				}
			},
			PostfixWithId(postfix) => {
				let var = scope.get_variable(scope_id, &postfix.expression).unwrap();
				match &var.var.kind {
					VariableKind::ModuleInstance(instance) => match &instance.kind {
						ModuleInstanceKind::Register(m) => match id_table.get_value(&postfix.id).as_str() {
							"data" => Ok(scope.get_api_id_by_internal_id(m.data).unwrap().into()),
							"en" => Ok(scope.get_api_id_by_internal_id(m.enable).unwrap().into()),
							"next" => Ok(scope.get_api_id_by_internal_id(m.next).unwrap().into()),
							"clk" => Ok(scope.get_api_id_by_internal_id(m.clk).unwrap().into()),
							"nreset" => Ok(scope.get_api_id_by_internal_id(m.nreset).unwrap().into()),
							_ => unreachable!(),
						},
						ModuleInstanceKind::Module(m) => {
							let var = m.interface.get(&postfix.id).unwrap();
							Ok(scope.get_api_id_by_internal_id(*var).unwrap().into())
						},
					},
					_ => unreachable!(),
				}
			},
			UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				let operand = unary
					.expression
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				match unary.code {
					LogicalNot => Ok(!operand),
					BitwiseNot => Ok(hirn::design::Expression::Unary(hirn::design::UnaryExpression {
						op: hirn::design::UnaryOp::BitwiseNot,
						operand: Box::new(operand),
					})),
					Minus => Ok(-operand),
					Plus => Ok(operand),
				}
			},
			UnaryCastExpression(unary_cast) => {
				let src = unary_cast
					.expression
					.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				let cast = additional_ctx.unwrap().casts.get(&self.get_location()).unwrap();
				use SignalSensitivity::*;
				let sensitivity = match &cast.dest_sensitivity {
					Async(_) => Some(hirn::design::SignalSensitivity::Async),
					Comb(list, _) => {
						let mut new_list = hirn::design::ClockSensitivityList::new_empty();
						for edge in &list.list {
							let id = scope.get_api_id_by_internal_id(edge.clock_signal).unwrap();
							let new_edge = hirn::design::EdgeSensitivity {
								clock_signal: id,
								on_rising: edge.on_rising,
							};
							new_list.push(new_edge);
						}
						Some(hirn::design::SignalSensitivity::Comb(new_list))
					},
					Sync(list, _) => {
						let mut new_list = hirn::design::ClockSensitivityList::new_empty();
						for edge in &list.list {
							let id = scope.get_api_id_by_internal_id(edge.clock_signal).unwrap();
							let new_edge = hirn::design::EdgeSensitivity {
								clock_signal: id,
								on_rising: edge.on_rising,
							};
							new_list.push(new_edge);
						}
						Some(hirn::design::SignalSensitivity::Sync(new_list))
					},
					Clock(..) => Some(hirn::design::SignalSensitivity::Clock),
					Const(_) => Some(hirn::design::SignalSensitivity::Const),
					NoSensitivity => None,
				};
				use SignalSignedness::*;
				let signedness = match &cast.dest_signedness {
					Signed(_) => Some(hirn::design::SignalSignedness::Signed),
					Unsigned(_) => Some(hirn::design::SignalSignedness::Unsigned),
					NoSignedness => None,
				};
				Ok(hirn::design::Expression::Cast(hirn::design::CastExpression {
					src: Box::new(src),
					signedness,
					sensitivity,
				}))
			},
			BinaryExpression(binop) => {
				use crate::parser::ast::BinaryOpcode::*;
				let lhs = binop.lhs.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				let rhs = binop.rhs.codegen(nc_table, id_table, scope_id, scope, additional_ctx)?;
				match binop.code {
					Multiplication => Ok(lhs * rhs),
					Division => Ok(lhs / rhs),
					Addition => Ok(lhs + rhs),
					Subtraction => Ok(lhs - rhs),
					Modulo => Ok(lhs % rhs),
					Equal => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::Equal,
						rhs: Box::new(rhs),
					})),
					NotEqual => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::NotEqual,
						rhs: Box::new(rhs),
					})),
					LShift => Ok(lhs << rhs),
					RShift => Ok(lhs >> rhs),
					BitwiseAnd => Ok(lhs & rhs),
					BitwiseOr => Ok(lhs | rhs),
					BitwiseXor => Ok(lhs ^ rhs),
					Less => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::Less,
						rhs: Box::new(rhs),
					})),
					Greater => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::Greater,
						rhs: Box::new(rhs),
					})),
					LessEqual => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::LessEqual,
						rhs: Box::new(rhs),
					})),
					GreaterEqual => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::GreaterEqual,
						rhs: Box::new(rhs),
					})),
					LogicalAnd => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::LogicalAnd,
						rhs: Box::new(rhs),
					})),
					LogicalOr => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::LogicalOr,
						rhs: Box::new(rhs),
					})),
				}
			},
		}
	}
	pub fn is_generic(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		scope_id: usize,
		local_ctx: &LocalAnalyzerContext,
	) -> miette::Result<bool> {
		use Expression::*;
		match self {
			Identifier(id) => match local_ctx.scope.get_variable(scope_id, &id.id) {
				Some(x) => match x.var.kind {
					crate::analyzer::VariableKind::Generic(_) => Ok(true),
					_ => Ok(false),
				},
				None => Err(miette::Report::new(
					SemanticError::VariableNotDeclared
						.to_diagnostic_builder()
						.label(id.location, "This variable is not defined in this scope")
						.build(),
				)),
			},
			PostfixWithIndex(id) => id.expression.is_generic(global_ctx, scope_id, local_ctx),
			PostfixWithId(module_inst) => {
				let m = local_ctx.scope.get_variable(scope_id, &module_inst.expression);
				match m {
					Some(var) => match &var.var.kind {
						crate::analyzer::VariableKind::ModuleInstance(instance) => match &instance.kind {
							crate::analyzer::ModuleInstanceKind::Module(m) => {
								for var in &m.interface {
									if var.0 == &module_inst.id {
										return Ok(local_ctx
											.scope
											.get_intermidiate_signal(*var.1)
											.var
											.kind
											.is_generic());
									}
								}
								Ok(false)
							},
							crate::analyzer::ModuleInstanceKind::Register(_) => Ok(false),
						},
						_ => {
							return Err(miette::Report::new(
								SemanticError::IdNotSubscriptable
									.to_diagnostic_builder()
									.label(module_inst.location, "This variable cannot be subscripted")
									.build(),
							))
						},
					},
					None => {
						return Err(miette::Report::new(
							SemanticError::VariableNotDeclared
								.to_diagnostic_builder()
								.label(module_inst.location, "This variable is not defined in this scope")
								.build(),
						))
					},
				}
			},
			_ => Ok(false),
		}
	}
	pub fn evaluate_type(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		scope_id: usize,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		coupling_type: Signal,
		is_lhs: bool,
		location: SourceSpan,
	) -> miette::Result<Signal> {
		use Expression::*;
		match self {
			Number(num) => {
				debug!("Number is {:?}", num.location);
				// if coupling type is none, width must be known
				let width = coupling_type.width();
				let key = &num.key;
				let mut constant = match local_ctx.nc_widths.get(&self.get_location()) {
						Some(nc) => {
							if local_ctx.ncs_to_be_exted.contains_key(&self.get_location()){
								global_ctx.nc_table.get_by_key(key).unwrap().clone()
							}
							else {
								nc.clone()
							}
						}
						None => global_ctx.nc_table.get_by_key(key).unwrap().clone(),
					};
				let mut sig = Signal::new_from_constant(&constant, num.location);
				match (width, sig.width()) {
					(None, None) => {},
					(None, Some(_)) => (),
					(Some(val), None) => {
						debug!("Setting width of {:?} to {:?} in local_ctx", constant, val);
						match val.get_value() {
							Some(value) => {
								constant.width = Some(value.to_u32().unwrap());
								log::debug!("NC is now {:?}", constant);
								sig.set_width(
									BusWidth::Evaluated(NumericConstant::from_u64(
										constant.width.unwrap() as u64,
										None,
										None,
										None,
									)),
									sig.get_signedness(),
									location,
								);
								local_ctx.nc_widths.insert(self.get_location(), constant.clone());
								if let Some(loc) = val.get_location() {
									log::debug!("Inserting {:?} to be extended", loc);
									local_ctx.ncs_to_be_exted.insert(self.get_location(), loc);
								}
							},
							None => {
								log::debug!("Inserting {:?} to be extended", self.get_location());
								local_ctx
									.ncs_to_be_exted
									.insert(self.get_location(), val.get_location().unwrap());
								log::debug!("ncs_to_be_exted is now {:?}", local_ctx.ncs_to_be_exted);
								sig.set_width(val, sig.get_signedness(), location);
							},
						}
					},
					(Some(coming), Some(original)) => {
						log::debug!("Coming: {:?}", coming);
						log::debug!("Original: {:?}", original);
						if coming != original {
							return Err(miette::Report::new(
								SemanticError::WidthMismatch
									.to_diagnostic_builder()
									.label(
										num.location,
										format!("Width of this expression is {}", original.get_value().unwrap())
											.as_str(),
									)
									.label(location, "Width mismach in this expression")
									.label(
										coupling_type.get_width_location().unwrap(),
										format!("Width of this expression is {}", coming.get_value().unwrap()).as_str(),
									)
									.build(),
							));
						}
					},
				}
				use crate::analyzer::SignalSignedness::*;
				log::debug!("After width constant is {:?}", constant);
				if coupling_type.is_signedness_specified() {
					match (coupling_type.get_signedness(), sig.get_signedness()) {
						(Signed(_), Signed(_)) | (Unsigned(_), Unsigned(_)) => (),
						(Signed(loc1), Unsigned(loc2)) | (Unsigned(loc1), Signed(loc2)) => {
							return Err(miette::Report::new(
								SemanticError::SignednessMismatch
									.to_diagnostic_builder()
									.label(location, "Signedness of this expression does not match")
									.label(loc1, "Signedness of this signal")
									.label(loc2, "Signedness of this expression")
									.build(),
							))
						},
						(NoSignedness, _) => (),
						(Signed(loc), NoSignedness) => {
							constant.signed = Some(true);
							sig.set_signedness(SignalSignedness::Signed(loc), loc);
							local_ctx.nc_widths.insert(self.get_location(), constant.clone());
						},
						(Unsigned(loc), NoSignedness) => {
							constant.signed = Some(false);
							log::debug!("Setting signedness of unsigned");
							sig.set_signedness(SignalSignedness::Unsigned(loc), loc);
							log::debug!("Constant is now {:?}", constant);
							local_ctx.nc_widths.insert(self.get_location(), constant.clone());
						},
					}
				}
				Ok(sig)
			},
			Identifier(identifier) => {
				let mut var = match local_ctx.scope.get_variable(scope_id, &identifier.id) {
					Some(var) => var,
					None => {
						return Err(miette::Report::new(
							SemanticError::VariableNotDeclared
								.to_diagnostic_builder()
								.label(identifier.location, "This variable is not defined in this scope")
								.build(),
						))
					},
				}
				.clone();
				let mut sig = var.var.kind.to_signal().map_err(|err| {
					err.label(self.get_location(), "This identifier cannot represents a signal")
						.build()
				})?;
				sig.evaluate_as_lhs(is_lhs, global_ctx, coupling_type, location)?;
				if var.var.kind == crate::analyzer::VariableKind::Signal(sig.clone()) {
					return Ok(sig);
				}
				var.var.kind = crate::analyzer::VariableKind::Signal(sig.clone());
				local_ctx.scope.redeclare_variable(var);
				Ok(sig)
			},
			ParenthesizedExpression(expr) => {
				expr.expression
					.evaluate_type(global_ctx, scope_id, local_ctx, coupling_type, is_lhs, location)
			},
			MatchExpression(match_expr) => {
				if match_expr.get_default().is_none() {
					return Err(miette::Report::new(
						SemanticError::ConditionalWithoutDefault
							.to_diagnostic_builder()
							.label(self.get_location(), "Match expression must have a default branch")
							.build(),
					));
				}
				let val = match_expr.value.evaluate_type(
					global_ctx,
					scope_id,
					local_ctx,
					Signal::new_empty(),
					is_lhs,
					location,
				)?;
				if val.is_array() {
					return Err(miette::Report::new(
						SemanticError::ArrayInExpression
							.to_diagnostic_builder()
							.label(self.get_location(), "Match expression cannot be used on arrays")
							.build(),
					));
				}
				let mut res = Signal::new_empty();
				let mut present_already = HashMap::new();
				for stmt in &match_expr.statements {
					match &stmt.antecedent {
						MatchExpressionAntecendent::Expression {
							expressions,
							location: _,
						} => {
							for expr in expressions {
								let value = expr.evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?.unwrap();
								if let Some(prev) = present_already.insert(value.value.clone(), expr.get_location()) {
									return Err(miette::Report::new(
										SemanticError::DuplicateMatchValue
											.to_diagnostic_builder()
											.label(prev, "Value is already present here in this match expression")
											.label(
												expr.get_location(),
												"This value is already present in this match expression",
											)
											.build(),
									));
								}
								let _ = expr.evaluate_type(
									//FIXME
									global_ctx,
									scope_id,
									local_ctx,
									val.clone(),
									is_lhs,
									match_expr.location,
								)?;
							}
						},
						MatchExpressionAntecendent::Default { location: _ } => (),
					};
					stmt.expression.evaluate_type(
						global_ctx,
						scope_id,
						local_ctx,
						coupling_type.clone(),
						is_lhs,
						location,
					)?;
					res = stmt.expression.evaluate_type(
						global_ctx,
						scope_id,
						local_ctx,
						coupling_type.clone(),
						is_lhs,
						match_expr.location,
					)?;
				}
				Ok(res)
			},
			ConditionalExpression(cond) => {
				if cond.get_default().is_none() {
					return Err(miette::Report::new(
						SemanticError::ConditionalWithoutDefault
							.to_diagnostic_builder()
							.label(self.get_location(), "Conditional expression must have a default branch")
							.build(),
					));
				}
				let mut res = Signal::new_empty();
				for stmt in &cond.statements {
					match &stmt.antecedent {
						MatchExpressionAntecendent::Expression {
							expressions,
							location: _,
						} => {
							for expr in expressions {
								expr.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_wire(expr.get_location()),
									is_lhs,
									cond.location,
								)?;
							}
						},
						MatchExpressionAntecendent::Default { location: _ } => (),
					};
					stmt.expression.evaluate_type(
						global_ctx,
						scope_id,
						local_ctx,
						coupling_type.clone(),
						is_lhs,
						location,
					)?;
					res = stmt.expression.evaluate_type(
						global_ctx,
						scope_id,
						local_ctx,
						coupling_type.clone(),
						is_lhs,
						cond.location,
					)?;
				}
				Ok(res)
			},
			Tuple(tuple) => {
				return Err(miette::Report::new(
					crate::CompilerError::FeatureNotImplementedYet
						.to_diagnostic_builder()
						.label(tuple.location, "Tuple is not implemented yet")
						.build(),
				))
			},
			TernaryExpression(ternary) => {
				let type_condition = ternary.condition.evaluate_type(
					global_ctx,
					scope_id,
					local_ctx,
					Signal::new_wire(self.get_location()),
					is_lhs,
					self.get_location(),
				)?;
				debug!("condition: {:?}", type_condition);
				let true_branch_type = ternary.true_branch.evaluate_type(
					global_ctx,
					scope_id,
					local_ctx,
					coupling_type.clone(),
					is_lhs,
					location,
				)?;
				debug!("true branch: {:?}", true_branch_type);
				let false_branch_type = ternary.false_branch.evaluate_type(
					global_ctx,
					scope_id,
					local_ctx,
					true_branch_type.clone(),
					is_lhs,
					location,
				)?;
				debug!("false branch: {:?}", false_branch_type);
				match (true_branch_type.width(), false_branch_type.width()) {
					(None, None) => {
						return Err(miette::Report::new(
							SemanticError::WidthNotKnown
								.to_diagnostic_builder()
								.label(
									ternary.true_branch.get_location(),
									"Width of this expression is unknown",
								)
								.label(
									ternary.false_branch.get_location(),
									"Width of this expression is unknown",
								)
								.build(),
						))
					},
					(None, Some(_)) => {
						ternary.true_branch.evaluate_type(
							global_ctx,
							scope_id,
							local_ctx,
							false_branch_type.clone(),
							is_lhs,
							location,
						)?;
					},
					(Some(_), None) => unreachable!(),
					(Some(_), Some(_)) => (), // This is checked by evaluating type second with the first one
				}
				match (
					true_branch_type.get_signedness().is_none(),
					false_branch_type.get_signedness().is_none(),
				) {
					(true, true) => {
						return Err(miette::Report::new(
							SemanticError::SignednessMismatch // FIXME when fixing error messages
								.to_diagnostic_builder()
								.label(
									ternary.true_branch.get_location(),
									"Signedness of this expression is unknown",
								)
								.label(
									ternary.false_branch.get_location(),
									"Signedness of this expression is unknown",
								)
								.build(),
						));
					},
					(false, true) => unreachable!(),
					(true, false) => {
						ternary.true_branch.evaluate_type(
							global_ctx,
							scope_id,
							local_ctx,
							false_branch_type.clone(),
							is_lhs,
							location,
						)?;
					},
					(false, false) => (),
				}
				Ok(false_branch_type)
			},
			PostfixWithIndex(index) => {
				let mut expr = index.expression.evaluate_type(
					global_ctx,
					scope_id,
					local_ctx,
					Signal::new_empty(),
					is_lhs,
					location,
				)?;
				if !expr.is_array() && !expr.is_bus() {
					return Err(miette::Report::new(
						SemanticError::ExpressionNonIndexable
							.to_diagnostic_builder()
							.label(index.location, "This expression cannot be indexed")
							.build(),
					));
				}
				let ind = index.index.evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?;
				if expr.is_array() {
					if let Some(val) = &ind {
						if val.value < BigInt::from(0)
							&& val.value >= expr.dimensions.last().unwrap().get_value().clone().unwrap()
						{
							return Err(miette::Report::new(
								SemanticError::IndexOutOfBounds
									.to_diagnostic_builder()
									.label(index.location, "Index is out of bounds")
									.build(),
							));
						}
					}
					local_ctx.array_or_bus.insert(self.get_location(), true);
					expr.dimensions.pop();
					return Ok(expr);
				}
				if let SignalType::Bus(bus) = &expr.signal_type {
					match &bus.width.clone().unwrap().get_value() {
						// FIXME is this unwrap safe?
						Some(val) => {
							if let Some(nc) = &ind {
								debug!("Index value is known: {:?}", nc);
								if nc.value < BigInt::from(0) || nc.value > val - 1 {
									return Err(miette::Report::new(
										SemanticError::IndexOutOfBounds
											.to_diagnostic_builder()
											.label(index.location, "Index is out of bounds")
											.label(
												index.index.get_location(),
												format!(
													"Index value should be between [0, {:?}] but is in fact {:?}",
													val - 1,
													nc.value
												)
												.as_str(),
											)
											.build(),
									));
								}
							}
							else {
								debug!("Index value is not known: {:?}", ind);
							}
						},
						None => (),
					}
					local_ctx.array_or_bus.insert(self.get_location(), false);
					expr.set_width(
						BusWidth::Evaluated(crate::core::NumericConstant::new_true()),
						bus.signedness.clone(),
						index.location,
					);
					return Ok(expr);
				}
				unreachable!()
			},
			PostfixWithRange(range) => {
				let mut expr = range.expression.evaluate_type(
					global_ctx,
					scope_id,
					local_ctx,
					Signal::new_empty(),
					is_lhs,
					location,
				)?;
				if expr.dimensions.len() > 0 {
					return Err(miette::Report::new(
						SemanticError::RangeOnNonBus
							.to_diagnostic_builder()
							.label(
								range.expression.get_location(),
								"This signal is an array, it cannot be range indexed",
							)
							.build(),
					));
				}
				use SignalType::*;
				match &expr.signal_type {
					Bus(bus) => {
						let begin: Option<NumericConstant> =
							range
								.range
								.lhs
								.evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?;
						let mut end = range
							.range
							.rhs
							.evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?;
						use crate::parser::ast::RangeOpcode::*;
						match range.range.code {
							Colon => (),
							PlusColon => {
								end = NumericConstant::new_from_binary(end.clone(), begin.clone(), |e1, e2| e1 + e2)
							},
							ColonLessThan => {
								end = NumericConstant::new_from_binary(
									end.clone(),
									Some(NumericConstant::new_true()),
									|e1, e2| e1 - e2,
								)
							},
						}
						match &bus.width {
							Some(val) => {
								match &val.get_value() {
									Some(value) => {
										let id = local_ctx.scope.add_expression(scope_id, self.clone());
										if let Some(begin_value) = &begin {
											if &begin_value.value > value || begin_value.value < 0.into() {
												return Err(miette::Report::new(
													SemanticError::WidthMismatch
														.to_diagnostic_builder()
														.label(
															range.location,
															"Cannot perform range indexing - range is out of bounds",
														)
														.label(
															range.location,
															format!(
																"Range bounds are: {}:... but actual width is: {:?}",
																begin_value.value, value
															)
															.as_str(),
														)
														.build(),
												));
											}
											if let Some(end_value) = &end {
												if &end_value.value > value || end_value.value < begin_value.value {
													return Err(miette::Report::new(
														SemanticError::WidthMismatch.to_diagnostic_builder()
															.label(range.location, "Cannot perform range indexing - range is out of bounds")
															.label(range.location, format!("Range bounds are: {}:{} but actual width is: {:?}", begin_value.value, end_value.value, value).as_str())
															.build(),
													));
												}
												expr.set_width(
													crate::analyzer::BusWidth::Evaluated(NumericConstant::new(
														end_value.clone().value - begin_value.clone().value
															+ BigInt::from(1),
														None,
														None,
														None,
													)),
													bus.signedness.clone(),
													range.location,
												);
											}
											else {
												expr.set_width(
													crate::analyzer::BusWidth::Evaluable(id),
													bus.signedness.clone(),
													range.location,
												);
											}
										}
										else {
											expr.set_width(
												crate::analyzer::BusWidth::Evaluable(id),
												bus.signedness.clone(),
												range.location,
											);
										}
									},
									None => (),
								}

								Ok(expr)
							},
							None => {
								return Err(miette::Report::new(
									SemanticError::WidthNotKnown
										.to_diagnostic_builder()
										.label(
											range.location,
											"Width of this expression is not known, but it should be",
										)
										.build(),
								))
							},
						}
					},
					Wire(type_loc) | Auto(type_loc) => {
						return Err(miette::Report::new(
							SemanticError::RangeOnNonBus
								.to_diagnostic_builder()
								.label(range.location, "Range can only be applied to a bus")
								.label(*type_loc, "This signal is not a bus")
								.build(),
						));
					},
				}
			},
			PostfixWithArgs(function) => {
				let func_name = global_ctx.id_table.get_value(&function.id);
				match func_name.as_str() {
					"zeros" | "ones" => {
						match function.argument_list.len() {
							//0 => { // FIXME
							//	Ok(coupling_type.clone())
							//},
							1 => {
								let expr = function.argument_list[0]
									.evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?
									.expect("This panics in generic modules implementatation"); // FIXME
								if expr.value < 0.into() {
									return Err(miette::Report::new(
										SemanticError::NegativeBusWidth // FIXME this error name
											.to_diagnostic_builder()
											.label(
												function.argument_list[0].get_location(),
												"This argument cannot be negative",
											)
											.build(),
									));
								}
								let expr = Signal::new_bus(
									Some(BusWidth::Evaluated(expr)),
									SignalSignedness::Unsigned(self.get_location()),
									self.get_location(),
								);
								Ok(expr)
							},
							_ => Err(miette::Report::new(
								SemanticError::BadFunctionArguments
									.to_diagnostic_builder()
									.label(function.location, "This function should have only one argument")
									.build(),
							)),
						}
					},
					"trunc" => {
						if function.argument_list.len() != 1 {
							return Err(miette::Report::new(
								SemanticError::BadFunctionArguments
									.to_diagnostic_builder()
									.label(function.location, "This function should have only one argument")
									.build(),
							));
						}
						if !coupling_type.is_width_specified() {
							return Err(miette::Report::new(
								SemanticError::WidthNotKnown
									.to_diagnostic_builder()
									.label(
										function.location,
										"This function must know the width of the left hand side signal",
									)
									.build(),
							));
						}
						let mut expr = function.argument_list[0].evaluate_type(
							global_ctx,
							scope_id,
							local_ctx,
							Signal::new_empty(),
							is_lhs,
							location,
						)?;
						if !expr.is_width_specified() {
							return Err(miette::Report::new(
								SemanticError::WidthNotKnown
									.to_diagnostic_builder()
									.label(
										function.argument_list[0].get_location(),
										"This function must know the width of argument",
									)
									.build(),
							));
						}
						match (
							expr.width().unwrap().get_value(),
							coupling_type.width().unwrap().get_value(),
						) {
							(Some(val1), Some(val2)) => {
								if val1 < val2 {
									return Err(miette::Report::new(
										SemanticError::WidthMismatch
											.to_diagnostic_builder()
											.label(
												function.argument_list[0].get_location(),
												"You cannot enlarge the width of the signal",
											)
											.build(),
									));
								}
							},
							_ => (),
						}
						use SignalSignedness::*;
						match (expr.get_signedness(), coupling_type.get_signedness()) {
							(Signed(_), Signed(_)) | (Unsigned(_), Unsigned(_)) => (),
							(Signed(loc_signed), Unsigned(loc_unsigned))
							| (Unsigned(loc_unsigned), Signed(loc_signed)) => {
								return Err(miette::Report::new(
									SemanticError::SignednessMismatch
										.to_diagnostic_builder()
										.label(self.get_location(), "Signedness of this expression does not match")
										.label(loc_signed, "This signedness is signed")
										.label(loc_unsigned, "This signedness is unsigned")
										.build(),
								))
							},
							(_, NoSignedness) => (), //FIXME
							(NoSignedness, _) => {
								function.argument_list[0].evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, coupling_type.get_signedness(), location),
									is_lhs,
									location,
								)?;
							},
						};
						local_ctx
							.scope
							.widths
							.insert(function.location, coupling_type.width().unwrap());
						expr.set_width(coupling_type.width().unwrap(), expr.get_signedness(), location);
						Ok(expr)
					},
					"zext" | "ext" | "sext" => {
						if function.argument_list.len() != 1 {
							return Err(miette::Report::new(
								SemanticError::BadFunctionArguments
									.to_diagnostic_builder()
									.label(function.location, "This function should have only one argument")
									.build(),
							));
						}
						if !coupling_type.is_width_specified() {
							return Err(miette::Report::new(
								SemanticError::WidthNotKnown
									.to_diagnostic_builder()
									.label(
										function.location,
										"This function must know the width of the left hand side signal",
									)
									.build(),
							));
						}
						let expr = function.argument_list[0].evaluate_type(
							global_ctx,
							scope_id,
							local_ctx,
							Signal::new_empty(),
							is_lhs,
							location,
						)?;
						if !expr.is_width_specified() {
							return Err(miette::Report::new(
								SemanticError::WidthNotKnown
									.to_diagnostic_builder()
									.label(
										function.argument_list[0].get_location(),
										"This function must know the width of argument",
									)
									.build(),
							));
						}
						match (
							expr.width().unwrap().get_value(),
							coupling_type.width().unwrap().get_value(),
						) {
							(Some(val1), Some(val2)) => {
								if val1 > val2 {
									return Err(miette::Report::new(
										SemanticError::WidthMismatch
											.to_diagnostic_builder()
											.label(
												function.argument_list[0].get_location(),
												"You cannot shrink the width of the signal",
											)
											.build(),
									));
								}
							},
							_ => (),
						}
						local_ctx
							.scope
							.widths
							.insert(function.location, coupling_type.width().unwrap());
						let signedness = match func_name.as_str() {
							"zext" => SignalSignedness::Unsigned(self.get_location()),
							"ext" => expr.get_signedness(),
							"sext" => SignalSignedness::Signed(self.get_location()),
							_ => unreachable!(),
						};
						let r_type = Signal::new_bus(coupling_type.width(), signedness, location);
						Ok(r_type)
					},
					"join" => {
						if function.argument_list.is_empty() {
							return Err(miette::Report::new(
								SemanticError::BadFunctionArguments
									.to_diagnostic_builder()
									.label(function.location, "This function should at least one argument")
									.build(),
							));
						}
						let mut t = Signal::new_empty();
						let mut nc = NumericConstant::new_from_value(0.into());
						for sig in &function.argument_list {
							let n_sig = sig.evaluate_type(
								global_ctx,
								scope_id,
								local_ctx,
								Signal::new_empty(),
								false,
								location,
							)?;
							if n_sig.is_array() {
								return Err(miette::Report::new(
									SemanticError::ArrayInExpression
										.to_diagnostic_builder()
										.label(sig.get_location(), "This function cannot be applied to an array")
										.build(),
								));
							}
							use SignalSignedness::*;
							if let Signed(loc) = n_sig.get_signedness() {
								return Err(miette::Report::new(
									SemanticError::SignednessMismatch
										.to_diagnostic_builder()
										.label(
											self.get_location(),
											"This function cannot be applied to a signed signal",
										)
										.label(loc, "This signal is signed")
										.build(),
								));
							}
							if !n_sig.is_width_specified() {
								return Err(miette::Report::new(
									SemanticError::WidthNotKnown
										.to_diagnostic_builder()
										.label(sig.get_location(), "This function must know the width of argument")
										.build(),
								));
							}
							nc.value += n_sig.width().unwrap().get_value().unwrap();
						}
						t.set_width(
							BusWidth::Evaluated(nc),
							SignalSignedness::Unsigned(self.get_location()),
							location,
						);
						Ok(t)
					},
					"rep" => {
						if function.argument_list.len() != 2 {
							return Err(miette::Report::new(
								SemanticError::BadFunctionArguments
									.to_diagnostic_builder()
									.label(function.location, "This function should have two arguments")
									.build(),
							));
						}
						let mut expr = function.argument_list[0].evaluate_type(
							global_ctx,
							scope_id,
							local_ctx,
							coupling_type,
							is_lhs,
							location,
						)?;
						let entry = EvaluatedEntry {
							expression: self.clone(),
							scope_id,
						};
						let id = local_ctx.scope.add_expression(scope_id, self.clone());
						let mut width = BusWidth::WidthOf(id);
						if let Some(count) =
							function.argument_list[1].evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?
						{
							if count.value < BigInt::from(1) {
								return Err(miette::Report::new(
									SemanticError::BadFunctionArguments
										.to_diagnostic_builder()
										.label(
											function.argument_list[1].get_location(),
											"This function cannot be applied to a negative number",
										)
										.build(),
								));
							}
							if let Some(w) = expr.width() {
								width = BusWidth::Evaluated(NumericConstant::new_from_value(
									w.get_value().unwrap() * count.value,
								));
							}
						}
						if expr.is_array() {
							return Err(miette::Report::new(
								SemanticError::ArrayInExpression
									.to_diagnostic_builder()
									.label(
										function.argument_list[0].get_location(),
										"This function cannot be applied to an array",
									)
									.build(),
							));
						}
						if !expr.is_width_specified() {
							return Err(miette::Report::new(
								SemanticError::WidthNotKnown
									.to_diagnostic_builder()
									.label(
										function.argument_list[0].get_location(),
										"This function must know the width of argument",
									)
									.build(),
							));
						}
						expr.set_width(width, expr.get_signedness(), location);
						Ok(expr)
					},
					"fold_or" | "fold_xor" | "fold_and" => {
						if function.argument_list.len() != 1 {
							return Err(miette::Report::new(
								SemanticError::BadFunctionArguments
									.to_diagnostic_builder()
									.label(function.location, "This function should have only one argument")
									.build(),
							));
						}
						let mut arg_type = function.argument_list[0].evaluate_type(
							global_ctx,
							scope_id,
							local_ctx,
							Signal::new_empty(),
							is_lhs,
							location,
						)?;
						if arg_type.is_array() {
							return Err(miette::Report::new(
								SemanticError::ArrayInExpression
									.to_diagnostic_builder()
									.label(
										function.argument_list[0].get_location(),
										"This function cannot be applied to an array",
									)
									.build(),
							));
						}
						if !arg_type.is_bus() {
							return Err(miette::Report::new(
								SemanticError::BadFunctionArguments
									.to_diagnostic_builder()
									.label(
										function.argument_list[0].get_location(),
										"This function cannot be applied to a bus",
									)
									.build(),
							));
						}
						arg_type.set_width(
							BusWidth::Evaluated(NumericConstant::new_from_value(1.into())),
							arg_type.get_signedness(),
							location,
						);
						Ok(arg_type)
					},
					_ => Err(miette::Report::new(
						SemanticError::UnknownBuiltInFunction
							.to_diagnostic_builder()
							.label(function.location, "Unknown builtin function")
							.build(),
					)),
				}
			},
			PostfixWithId(module) => {
				let m = local_ctx.scope.get_variable(scope_id, &module.expression);
				match m {
					Some(var) => match &var.var.kind {
						crate::analyzer::VariableKind::ModuleInstance(m) => {
							use ModuleInstanceKind::*;
							match &m.kind {
								Module(m) => match m.interface.get(&module.id) {
									Some(id) => {
										let sig = local_ctx.scope.get_intermidiate_signal(*id);
										return Ok(sig.var.kind.to_signal().unwrap());
									},
									None => {
										return Err(miette::Report::new(
											SemanticError::IdNotSubscriptable
												.to_diagnostic_builder()
												.label(
													self.get_location(),
													"This variable is not part of this module interface",
												)
												.build(),
										));
									},
								},
								Register(reg) => match global_ctx.id_table.get_value(&module.id).as_str() {
									"data" => {
										let var = local_ctx.scope.get_variable_by_id(reg.data).unwrap();
										return Ok(var.var.kind.to_signal().unwrap());
									},
									"next" => {
										let var = local_ctx.scope.get_variable_by_id(reg.next).unwrap();
										return Ok(var.var.kind.to_signal().unwrap());
									},
									"en" => {
										let var = local_ctx.scope.get_variable_by_id(reg.enable).unwrap();
										return Ok(var.var.kind.to_signal().unwrap());
									},
									"clk" => {
										let var = local_ctx.scope.get_variable_by_id(reg.clk).unwrap();
										return Ok(var.var.kind.to_signal().unwrap());
									},
									"nreset" => {
										let var = local_ctx.scope.get_variable_by_id(reg.nreset).unwrap();
										return Ok(var.var.kind.to_signal().unwrap());
									},
									_ => {
										return Err(miette::Report::new(
											SemanticError::IdNotSubscriptable
												.to_diagnostic_builder()
												.label(
													self.get_location(),
													"This variable is not part of this module interface",
												)
												.build(),
										));
									},
								},
							}
						},
						_ => {
							return Err(miette::Report::new(
								SemanticError::IdNotSubscriptable
									.to_diagnostic_builder()
									.label(module.location, "This variable cannot be subscripted")
									.build(),
							))
						},
					},
					None => {
						return Err(miette::Report::new(
							SemanticError::VariableNotDeclared
								.to_diagnostic_builder()
								.label(module.location, "This variable is not defined in this scope")
								.build(),
						))
					},
				}
			},
			UnaryOperatorExpression(unary) => {
				let expr =
					unary
						.expression
						.evaluate_type(global_ctx, scope_id, local_ctx, coupling_type, is_lhs, location)?;
				if expr.is_array() {
					return Err(miette::Report::new(
						SemanticError::ArrayInExpression
							.to_diagnostic_builder()
							.label(
								unary.location,
								"This expression is an array, it is not allowed in binary expression",
							)
							.build(),
					));
				}
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					BitwiseNot => (),
					LogicalNot => (),
					Minus => todo!(), // FIXME ???
					Plus => (),
				}
				Ok(expr)
			},
			UnaryCastExpression(cast) => {
				if cast.type_name.array_declarators.len() != 0 {
					return Err(miette::Report::new(
						SemanticError::BadCast
							.to_diagnostic_builder()
							.label(
								cast.type_name.location,
								"This expression is an array, it is not allowed to be casted on",
							)
							.label(cast.type_name.location, "Type declared as an array here")
							.build(),
					));
				}
				let expr = cast.expression.evaluate_type(
					global_ctx,
					scope_id,
					local_ctx,
					Signal::new_bus(coupling_type.width(), SignalSignedness::NoSignedness, location),
					is_lhs,
					location,
				)?;
				if expr.is_array() {
					return Err(miette::Report::new(
						SemanticError::BadCast
							.to_diagnostic_builder()
							.label(
								cast.expression.get_location(),
								"This expression is an array, it is not allowed to be casted",
							)
							.label(cast.expression.get_location(), "Type declared as an array here")
							.build(),
					));
				}
				if expr.is_auto() {
					return Err(miette::Report::new(
						SemanticError::BadCast
							.to_diagnostic_builder()
							.label(
								cast.expression.get_location(),
								"This expression is an auto type, it is not allowed to be casted",
							)
							.label(cast.expression.get_location(), "Type declared as an auto here")
							.build(),
					));
				}
				if !expr.is_width_specified() {
					return Err(miette::Report::new(
						SemanticError::WidthNotKnown
							.to_diagnostic_builder()
							.label(
								cast.expression.get_location(),
								"Width of this expression is not known, but it should be",
							)
							.build(),
					));
				}
				let kind = VariableKind::from_type_declarator(
					&cast.type_name.declarator,
					scope_id,
					AlreadyCreated::new(),
					global_ctx.nc_table,
					global_ctx.id_table,
					&mut local_ctx.scope,
				)?;
				match &kind {
					VariableKind::Signal(sig) => {
						if sig.is_width_specified() {
							if sig.width().unwrap().get_value().unwrap() != expr.width().unwrap().get_value().unwrap() {
								return Err(miette::Report::new(
									SemanticError::BadCast
										.to_diagnostic_builder()
										.label(
											cast.type_name.location,
											"Casting to type with different width is not allowed",
										)
										.label(sig.get_width_location().unwrap(), "Width of this type")
										.label(expr.get_width_location().unwrap(), "Width of this expression")
										.build(),
								));
							}
						}
						let mut r = sig.clone();
						let mut cast = Cast::new(self.get_location());
						if !r.is_sensititivity_specified() {
							r.sensitivity = expr.sensitivity.clone();
						}
						else {
							cast.add_sensitivity(r.sensitivity.clone())
						}
						if !r.is_signedness_specified() {
							r.set_signedness(expr.get_signedness(), location);
						}
						else {
							cast.add_signedness(r.get_signedness());
						}
						if !r.is_width_specified() {
							r.set_width(
								expr.width().expect("Width is checked not to be none"),
								r.get_signedness(),
								location,
							)
						}
						debug!("casted to: {:?}", r);
						local_ctx.casts.insert(self.get_location(), cast);
						Ok(r)
					},
					VariableKind::Generic(_) => {
						return Err(miette::Report::new(
							SemanticError::BadCast
								.to_diagnostic_builder()
								.label(cast.type_name.location, "Casting to generic type is not allowed")
								.build(),
						))
					},
					VariableKind::ModuleInstance(_) => unreachable!(),
				}
			},
			BinaryExpression(binop) => {
				use crate::parser::ast::BinaryOpcode::*;
				let mut type_first = binop.lhs.evaluate_type(
					global_ctx,
					scope_id,
					local_ctx,
					Signal::new_empty(),
					is_lhs,
					binop.location,
				)?;
				if type_first.is_array() {
					return Err(miette::Report::new(
						SemanticError::ArrayInExpression
							.to_diagnostic_builder()
							.label(
								binop.lhs.get_location(),
								"This expression is an array, it is not allowed in binary expression",
							)
							.build(),
					));
				}
				debug!("type_first: {:?}", type_first);
				let mut type_second = match binop.code.do_widths_have_to_match() {
					true => binop.rhs.evaluate_type(
						global_ctx,
						scope_id,
						local_ctx,
						type_first.clone(),
						is_lhs,
						binop.location,
					)?,
					false => binop.rhs.evaluate_type(
						global_ctx,
						scope_id,
						local_ctx,
						Signal::new_empty(),
						is_lhs,
						binop.location,
					)?,
				};
				debug!("type_second: {:?}", type_second);
				if type_second.is_array() {
					return Err(miette::Report::new(
						SemanticError::ArrayInExpression
							.to_diagnostic_builder()
							.label(
								binop.rhs.get_location(),
								"This expression is an array, it is not allowed in binary expression",
							)
							.build(),
					));
				}
				log::debug!("Code: {:?}", binop.code);
				let (w, s) = match &binop.code {
					Multiplication => {
						if !type_first.is_width_specified(){
							report_unknown_width(binop.lhs.get_location())?;
						}
						if !type_second.is_width_specified(){
							report_unknown_width(binop.rhs.get_location())?;
						}
						use SignalSignedness::*;
						match (&type_first.get_signedness(), &type_second.get_signedness()) {
							(Signed(_), Signed(_)) | (Unsigned(_), Unsigned(_)) => (),
							(Signed(loc1), Unsigned(loc2)) | (Unsigned(loc2), Signed(loc1)) => {
								return Err(miette::Report::new(
									SemanticError::SignednessMismatch
										.to_diagnostic_builder()
										.label(*loc1, "This signal is signed")
										.label(*loc2, "This signal is unsigned")
										.build(),
								));
							},
							(_, NoSignedness) => {
								binop.rhs.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, type_first.get_signedness(), self.get_location()),
									is_lhs,
									location,
								)?;
							},
							(NoSignedness, _) => {
								binop.lhs.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, type_second.get_signedness(), self.get_location()),
									is_lhs,
									location,
								)?;
								type_first.set_signedness(type_second.get_signedness(), self.get_location());
							},
						}
						let id = local_ctx.scope.add_expression(scope_id, self.clone());
						match (
							&type_first.width().unwrap().get_value(),
							&type_second.width().unwrap().get_value(),
						) {
							(None, None) => (BusWidth::WidthOf(id), type_first.get_signedness()),
							(None, Some(_)) => (BusWidth::WidthOf(id), type_first.get_signedness()),
							(Some(_), None) => (BusWidth::WidthOf(id), type_first.get_signedness()),
							(Some(v1), Some(v2)) => (BusWidth::Evaluated(NumericConstant::new_from_value(v1 + v2)), type_first.get_signedness()),
						}
					},
					Division => {
						type_first = binop.lhs.evaluate_type(global_ctx, scope_id, local_ctx, Signal::new_bus(coupling_type.width(), coupling_type.get_signedness(), location), is_lhs, location)?;
						if !type_first.is_width_specified(){
							report_unknown_width(binop.lhs.get_location())?;
						}
						(type_first.width().unwrap(), type_first.get_signedness())
					},
					Addition | Subtraction => {
						use SignalSignedness::*;
						if !type_first.is_width_specified(){
							report_unknown_width(binop.lhs.get_location())?;
						}
						if !type_second.is_width_specified(){
							report_unknown_width(binop.rhs.get_location())?;
						}
						match (&type_first.get_signedness(), &type_second.get_signedness()) {
							(Signed(_), Signed(_)) | (Unsigned(_), Unsigned(_)) => (),
							(Signed(loc1), Unsigned(loc2)) | (Unsigned(loc2), Signed(loc1)) => {
								return Err(miette::Report::new(
									SemanticError::SignednessMismatch
										.to_diagnostic_builder()
										.label(*loc1, "This signal is signed")
										.label(*loc2, "This signal is unsigned")
										.build(),
								));
							},
							(_, NoSignedness) => {
								binop.rhs.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, type_first.get_signedness(), self.get_location()),
									is_lhs,
									location,
								)?;
							},
							(NoSignedness, _) => {
								binop.lhs.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, type_second.get_signedness(), self.get_location()),
									is_lhs,
									location,
								)?;
								type_first.set_signedness(type_second.get_signedness(), self.get_location());
							},
						}
						let id = local_ctx.scope.add_expression(scope_id, self.clone());
						match (
							&type_first.width().unwrap().get_value(),
							&type_second.width().unwrap().get_value(),
						) {
							(None, None) => (BusWidth::WidthOf(id), type_first.get_signedness()),
							(None, Some(_)) => (BusWidth::WidthOf(id), type_first.get_signedness()),
							(Some(_), None) => (BusWidth::WidthOf(id), type_first.get_signedness()),
							(Some(v1), Some(v2)) => {
								(BusWidth::Evaluated(NumericConstant::new_from_value(max(v1, v2) + BigInt::from(1))), type_first.get_signedness())
							},
						}
					},
					Modulo => {
						type_second = binop.rhs.evaluate_type(global_ctx, scope_id, local_ctx, Signal::new_bus(coupling_type.width(), coupling_type.get_signedness(), location), is_lhs, location)?;
						if !type_second.is_width_specified(){
							report_unknown_width(binop.rhs.get_location())?;
						}
						(type_second.width().unwrap(), type_second.get_signedness())
					},
					LShift | RShift => {
						binop.rhs.evaluate_type(global_ctx, scope_id, local_ctx, Signal::new_bus(None, SignalSignedness::Unsigned(self.get_location()), location), is_lhs, location)?;
						if type_second.get_signedness().is_signed(){
							return Err(miette::Report::new(
								SemanticError::SignednessMismatch
									.to_diagnostic_builder()
									.label(*type_second.get_signedness().location().unwrap(), "This signal is signed")
									.build(),
							));
						}
						if !type_first.is_width_specified() {
							report_unknown_width(binop.lhs.get_location())?;
						}
						(type_first.width().clone().unwrap(), type_first.get_signedness())
					}
					BitwiseAnd | BitwiseOr | BitwiseXor => {
						binop.lhs.evaluate_type(global_ctx, scope_id, local_ctx, coupling_type, is_lhs, location)?;
						match (type_first.width(), type_second.width()){
    					    (None, None) => {
								report_unknown_width(self.get_location())?;
							},
    					    (None, Some(w)) => {
								binop.lhs.evaluate_type(global_ctx, scope_id, local_ctx, Signal::new_bus(Some(w), type_first.get_signedness(), location), is_lhs, location)?;
							},
    					    (Some(w), None) => {
								binop.rhs.evaluate_type(global_ctx, scope_id, local_ctx, Signal::new_bus(Some(w), type_second.get_signedness(), location), is_lhs, location)?;
							},
    					    (Some(w1), Some(w2)) => {
								if w1 != w2{
									return Err(miette::Report::new(
										SemanticError::WidthMismatch
											.to_diagnostic_builder()
											.label(
												binop.lhs.get_location(),
												"Width of this expression does not match",
											)
											.label(
												binop.rhs.get_location(),
												"Width of this expression does not match",
											)
											.build(),
									));
								}
							},
    					}
						use SignalSignedness::*;
						match (&type_first.get_signedness(), &type_second.get_signedness()) {
							(Signed(_), Signed(_)) | (Unsigned(_), Unsigned(_)) => (),
							(Signed(loc1), Unsigned(loc2)) | (Unsigned(loc2), Signed(loc1)) => {
								return Err(miette::Report::new(
									SemanticError::SignednessMismatch
										.to_diagnostic_builder()
										.label(*loc1, "This signal is signed")
										.label(*loc2, "This signal is unsigned")
										.build(),
								));
							},
							(_, NoSignedness) => {
								binop.rhs.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, type_first.get_signedness(), self.get_location()),
									is_lhs,
									location,
								)?;
							},
							(NoSignedness, _) => {
								binop.lhs.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, type_second.get_signedness(), self.get_location()),
									is_lhs,
									location,
								)?;
								type_first.set_signedness(type_second.get_signedness(), self.get_location());
							},
						}
						(type_first.width().unwrap(), type_first.get_signedness())
					},
					LogicalAnd | LogicalOr => {
						binop.lhs.evaluate_type(global_ctx, scope_id, local_ctx, Signal::new_wire(self.get_location()), is_lhs, location)?;
						binop.rhs.evaluate_type(global_ctx, scope_id, local_ctx, Signal::new_wire(self.get_location()), is_lhs, location)?;
						(BusWidth::Evaluated(NumericConstant::new_from_value(BigInt::from(1))), SignalSignedness::Unsigned(self.get_location()))
					},
					NotEqual | Equal | Less | Greater | LessEqual | GreaterEqual  => {
						use SignalSignedness::*;
						match (&type_first.get_signedness(), &type_second.get_signedness()) {
							(Signed(_), Signed(_)) | (Unsigned(_), Unsigned(_)) => (),
							(Signed(loc1), Unsigned(loc2)) | (Unsigned(loc2), Signed(loc1)) => {
								return Err(miette::Report::new(
									SemanticError::SignednessMismatch
										.to_diagnostic_builder()
										.label(*loc1, "This signal is signed")
										.label(*loc2, "This signal is unsigned")
										.build(),
								));
							},
							(_, NoSignedness) => {
								binop.rhs.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, type_first.get_signedness(), self.get_location()),
									is_lhs,
									location,
								)?;
							},
							(NoSignedness, _) => {
								binop.lhs.evaluate_type(
									global_ctx,
									scope_id,
									local_ctx,
									Signal::new_bus(None, type_second.get_signedness(), self.get_location()),
									is_lhs,
									location,
								)?;
								type_first.set_signedness(type_second.get_signedness(), self.get_location());
							},
						}
						(BusWidth::Evaluated(NumericConstant::new_from_value(BigInt::from(1))), SignalSignedness::Unsigned(self.get_location()))
					},
				};
				Ok(Signal::new_bus(
					Some(w),
					s,
					self.get_location(),
				))
			},
		}
	}

	pub fn can_create_binding_as_rhs(&self) -> bool {
		use Expression::*;
		match &self {
			Number(_) => false,
			Identifier(_) => true,
			ParenthesizedExpression(expr) => expr.expression.can_create_binding_as_rhs(),
			MatchExpression(_) => todo!(),
			ConditionalExpression(_) => todo!(),
			Tuple(_) => false,
			TernaryExpression(_) => false,
			PostfixWithIndex(_) => true,
			PostfixWithRange(_) => true,
			PostfixWithArgs(_) => true,
			PostfixWithId(_) => true,
			UnaryOperatorExpression(_) => true,
			UnaryCastExpression(_) => false,
			BinaryExpression(_) => false,
		}
	}
	pub fn get_dependencies(&self, scope_id: usize, local_ctx: &ModuleImplementationScope) -> Vec<InternalVariableId> {
		use Expression::*;
		match self {
			Number(_) => vec![],
			Identifier(id) => {
				log::debug!("id: {:?}", id.id);
				vec![local_ctx.get_variable(scope_id, &id.id).unwrap().id]
			},
			ParenthesizedExpression(expr) => expr.expression.get_dependencies(scope_id, local_ctx),
			MatchExpression(expr) => {
				let mut deps = expr.value.get_dependencies(scope_id, local_ctx);
				for stmt in &expr.statements {
					if let MatchExpressionAntecendent::Expression {
						expressions,
						location: _,
					} = &stmt.antecedent
					{
						for expr in expressions {
							deps.append(&mut expr.get_dependencies(scope_id, local_ctx));
						}
					}
					deps.append(&mut stmt.expression.get_dependencies(scope_id, local_ctx));
				}
				deps
			},
			ConditionalExpression(expr) => {
				let mut deps = Vec::new();
				for stmt in &expr.statements {
					if let MatchExpressionAntecendent::Expression {
						expressions,
						location: _,
					} = &stmt.antecedent
					{
						for expr in expressions {
							deps.append(&mut expr.get_dependencies(scope_id, local_ctx));
						}
					}
					deps.append(&mut stmt.expression.get_dependencies(scope_id, local_ctx));
				}
				deps
			},
			Tuple(_) => unreachable!(),
			TernaryExpression(expr) => {
				let mut deps = expr.condition.get_dependencies(scope_id, local_ctx);
				deps.append(&mut expr.true_branch.get_dependencies(scope_id, local_ctx));
				deps.append(&mut expr.false_branch.get_dependencies(scope_id, local_ctx));
				deps
			},
			PostfixWithIndex(expr) => {
				let mut deps = expr.expression.get_dependencies(scope_id, local_ctx);
				deps.append(&mut expr.index.get_dependencies(scope_id, local_ctx));
				deps
			},
			PostfixWithRange(expr) => {
				let mut deps = expr.expression.get_dependencies(scope_id, local_ctx);
				deps.append(&mut expr.range.lhs.get_dependencies(scope_id, local_ctx));
				deps.append(&mut expr.range.rhs.get_dependencies(scope_id, local_ctx));
				deps
			},
			PostfixWithArgs(function) => {
				let mut deps = Vec::new();
				for arg in function.argument_list.iter() {
					deps.append(&mut arg.get_dependencies(scope_id, local_ctx));
				}
				deps
			},
			PostfixWithId(expr) => todo!(),
			UnaryOperatorExpression(expr) => expr.expression.get_dependencies(scope_id, local_ctx),
			UnaryCastExpression(expr) => {
				let deps = expr.expression.get_dependencies(scope_id, local_ctx);
				deps // FIXME ID from sync/comb
			},
			BinaryExpression(expr) => {
				let mut deps = expr.lhs.get_dependencies(scope_id, local_ctx);
				deps.append(&mut expr.rhs.get_dependencies(scope_id, local_ctx));
				deps
			},
		}
	}
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Cast {
	pub location: SourceSpan,
	pub dest_signedness: SignalSignedness,
	pub dest_sensitivity: SignalSensitivity,
}
impl Cast {
	pub fn new(location: SourceSpan) -> Self {
		Self {
			location,
			dest_signedness: SignalSignedness::NoSignedness,
			dest_sensitivity: SignalSensitivity::NoSensitivity,
		}
	}
	pub fn add_signedness(&mut self, signedness: SignalSignedness) {
		self.dest_signedness = signedness;
	}
	pub fn add_sensitivity(&mut self, sensitivity: SignalSensitivity) {
		self.dest_sensitivity = sensitivity;
	}
}
fn report_not_allowed_expression(span: SourceSpan, expr_name: &str) -> miette::Result<Option<NumericConstant>> {
	Err(miette::Report::new(
		SemanticError::ExpressionNotAllowedInNonGenericModuleDeclaration
			.to_diagnostic_builder()
			.label(
				span,
				format!(
					"This {} expression is not allowed in non-generic module declaration",
					expr_name
				)
				.as_str(),
			)
			.build(),
	))
}

fn report_unknown_width(span: SourceSpan) -> miette::Result<Signal> {
	Err(miette::Report::new(
		SemanticError::WidthNotKnown
			.to_diagnostic_builder()
			.label(span, "Width of this expression is not known, but it should be")
			.build(),
	))
}
