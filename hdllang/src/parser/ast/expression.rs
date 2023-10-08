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

use crate::analyzer::{EdgeSensitivity, ModuleImplementationScope, SemanticError, Signal, SignalType, GlobalAnalyzerContext, LocalAnalyzerContex, BusWidth, SignalSensitivity};
use crate::lexer::IdTableKey;
use crate::parser::ast::{opcodes::*, MatchExpressionStatement, RangeExpression, SourceLocation, TypeName};
use crate::{ProvidesCompilerDiagnostic, SourceSpan};
pub use binary_expression::BinaryExpression;
pub use conditional_expression::ConditionalExpression;
use hirn::design::SignalSlice;
pub use identifier::Identifier;
pub use match_expression::MatchExpression;
use num_traits::Zero;
pub use number::Number;
pub use parenthesized::ParenthesizedExpression;
pub use postfix_with_args::PostfixWithArgs;
pub use postfix_with_id::PostfixWithId;
pub use postfix_with_index::PostfixWithIndex;
pub use postfix_with_range::PostfixWithRange;
use std::fmt::{Debug, Error, Formatter};
use std::vec;
pub use ternary_expression::TernaryExpression;
pub use tuple::Tuple;
pub use unary_cast_expression::UnaryCastExpression;
pub use unary_operator_expression::UnaryOperatorExpression;

use crate::core::NumericConstant;
use num_bigint::{BigInt, Sign};
#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
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
	pub fn create_edge_sensitivity(&self) -> miette::Result<EdgeSensitivity> {
		use self::Expression::*;
		match self {
			Identifier(id) => Ok(EdgeSensitivity {
				clock_signal: id.id,
				on_rising: true,
				location: id.location,
			}),
			UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					LogicalNot => unary.expression.create_edge_sensitivity().map(|mut edge| {
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
						return Err(miette::Report::new(
							SemanticError::VariableNotDeclared
								.to_diagnostic_builder()
								.label(id.location, "This variable is not defined in this scope")
								.build(),
						))
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
						Some(val) => Ok(Some(val.clone())),
						None =>{
							if let crate::analyzer::Direction::Input(_) = &generic.direction{
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
					}
					},
					ModuleInstance(_) => unreachable!(),
				}
			},
			Expression::ParenthesizedExpression(expr) => expr.expression.evaluate(nc_table, scope_id, scope),
			Expression::MatchExpression(expr) => report_not_allowed_expression(expr.location, "match"),
			Expression::ConditionalExpression(expr) => report_not_allowed_expression(expr.location, "conditional"),
			Expression::Tuple(_) => unreachable!(),
			Expression::TernaryExpression(tern) =>{
				let val = tern.condition.evaluate(nc_table, scope_id, scope)?;
				if val.is_none(){
					return Ok(None);
				}
				Ok(
				if tern.condition.evaluate(nc_table, scope_id, scope)?.unwrap().value != BigInt::from(0) {
					tern.true_branch.evaluate(nc_table, scope_id, scope)?
				} else {
					tern.false_branch.evaluate(nc_table, scope_id, scope)?
				},
			)},
			Expression::PostfixWithIndex(expr) => report_not_allowed_expression(expr.location, "index"),
			Expression::PostfixWithRange(expr) => report_not_allowed_expression(expr.location, "range"), // nie bedzie
			Expression::PostfixWithArgs(function) => {
				//let name: String = todo!("get name from id table key");
				//if name != String::from("sizeof"){
				//	return report_not_allowed_expression(function.location, "postfix with id");
				//}
				//if function.argument_list.len() != 1 {
				//	return Err(miette::Report::new(
				//		SemanticError::InvalidNumberOfArguments
				//			.to_diagnostic_builder()
				//			.label(
				//				function.location,
				//				"sizeof function takes exactly one argument",
				//			)
				//			.build(),
				//	));
				//}
				let arg = function.argument_list.first().unwrap();
				todo!()
				//let type_eval = arg.evaluate_type(global_ctx, scope_id, local_ctx, coupling_type, is_lhs, location)?;
			},                                                   // szerokość busa
			Expression::PostfixWithId(expr) => report_not_allowed_expression(expr.location, "postfix with id"), // nie bedzie
			Expression::UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				use crate::core::numeric_constant::*;
				match unary.code {
					LogicalNot => {
						let expr = unary.expression.evaluate(nc_table, scope_id, scope)?;
						if expr.is_none(){
							return Ok(None);
						}
						Ok(
						if unary.expression.evaluate(nc_table, scope_id, scope)?.unwrap().value == BigInt::from(0) {
							Some(NumericConstant::new(BigInt::from(1), None, None, Some(NumericConstantBase::Boolean)))
						} else {
							Some(NumericConstant::new(BigInt::from(0), None, None, Some(NumericConstantBase::Boolean)))
						},
					)},
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
				if lhs.is_none() || rhs.is_none(){
					return Ok(None);
				}
				use crate::parser::ast::BinaryOpcode::*;
				use crate::core::numeric_constant::*;
				match binop.code {
					Multiplication => Ok(NumericConstant::new_from_binary(
						lhs,
						rhs,
						|e1, e2| e1 * e2,
					)),
					Division => {
						if rhs.clone().unwrap().value.is_zero() {
							return Err(miette::Report::new(
								SemanticError::DivisionByZero
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Division by zero is not allowed")
									.build(),
							));
						}

						Ok(NumericConstant::new_from_binary(
							lhs,
							rhs,
							|e1, e2| e1 / e2,
						))
					},
					Addition => Ok(NumericConstant::new_from_binary(
						lhs,
						rhs,
						|e1, e2| e1 + e2,
					)),
					Subtraction => Ok(NumericConstant::new_from_binary(
						lhs,
						rhs,
						|e1, e2| e1 - e2,
					)),
					Modulo => {
						if rhs.is_some(){

							if rhs.clone().unwrap().value.is_zero() {
								return Err(miette::Report::new(
									SemanticError::DivisionByZero
										.to_diagnostic_builder()
										.label(binop.rhs.get_location(), "It is not allowed to modulo by zero")
										.build(),
								));
							}
						}
						Ok(NumericConstant::new_from_binary(
							lhs,
							rhs,
							|e1, e2| e1 / e2,
						))
					},
					Equal =>{
						
					Ok(
						
						if lhs.unwrap().value
							== rhs.unwrap().value
						{
							Some(NumericConstant::from_u64(1, Some(1), Some(false), Some(NumericConstantBase::Boolean)))
						} else {
							Some(NumericConstant::from_u64(0, Some(1), Some(false), Some(NumericConstantBase::Boolean)))
						},
					)},
					NotEqual => Ok(
						if lhs.unwrap().value
							!= rhs.unwrap().value
						{
							Some(NumericConstant::from_u64(1, Some(1), Some(false), Some(NumericConstantBase::Boolean)))
						} else {
							Some(NumericConstant::from_u64(0, Some(1), Some(false), Some(NumericConstantBase::Boolean)))
						},
					),
					LShift => {
						if rhs.clone().unwrap().value.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						} else {
							let mut i = BigInt::from(0);
							while i < rhs.clone().unwrap().value {
								lhs.clone().unwrap().value = lhs.clone().unwrap().value << 1;
								i += BigInt::from(1);
							}
							Ok(lhs)
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
						} else {
							let mut i = BigInt::from(0);
							while i < rhs.clone().unwrap().value {
								lhs.clone().unwrap().value = lhs.clone().unwrap().value >> 1;
								i += BigInt::from(1);
							}
							Ok(lhs)
						}
					},
					BitwiseAnd => Ok(NumericConstant::new_from_binary(
						lhs,
						rhs,
						|e1, e2| e1 & e2,
					)),
					BitwiseOr => Ok(NumericConstant::new_from_binary(
						lhs,
						rhs,
						|e1, e2| e1 | e2,
					)),
					BitwiseXor => Ok(NumericConstant::new_from_binary(
						lhs,
						rhs,
						|e1, e2| e1 ^ e2,
					)),
					Less => Ok(
						if lhs.unwrap().value
							< rhs.unwrap().value
						{
							Some(NumericConstant::new_true())
						} else {
							Some(NumericConstant::new_false())
						},
					),
					Greater => Ok(
						if lhs.unwrap().value
							> rhs.unwrap().value
						{
							Some(NumericConstant::new_true())
						} else {
							Some(NumericConstant::new_false())
						},
					),
					LessEqual => Ok(
						if lhs.unwrap().value
							<= rhs.unwrap().value
						{
							Some(NumericConstant::new_true())
						} else {
							Some(NumericConstant::new_false())
						},
					),
					GreaterEqual => Ok(
						if lhs.unwrap().value
							>= rhs.unwrap().value
						{
							Some(NumericConstant::new_true())
						} else {
							Some(NumericConstant::new_false())
						},
					),
					LogicalAnd => Ok(
						if lhs.unwrap().value != BigInt::from(0)
							&& rhs.unwrap().value != BigInt::from(0)
						{
							Some(NumericConstant::new_true())
						} else {
							Some(NumericConstant::new_false())
						},
					),
					LogicalOr => Ok(
						if lhs.unwrap().value != BigInt::from(0)
							|| rhs.unwrap().value != BigInt::from(0)
						{
							Some(NumericConstant::new_true())
						} else {
							Some(NumericConstant::new_false())
						},
					),
				}
			},
		}
	}
	pub fn get_slice(&self, nc_table: &crate::lexer::NumericConstantTable, scope_id: usize, scope: &ModuleImplementationScope) -> miette::Result<SignalSlice>{
		use self::Expression::*;
		match self {
			Identifier(id) => {
				let signal_id = scope.get_api_id(scope_id, &id.id).unwrap();
				Ok(SignalSlice{
					signal: signal_id,
					indices: vec![], 
				})
			},
			PostfixWithIndex(ind) => {
				let index_expr = ind.index.codegen(nc_table, scope_id, scope)?;
				let mut slice = ind.expression.get_slice(nc_table, scope_id, scope)?;
				slice.indices.push(index_expr);
				Ok(slice)
			},
			_ => unreachable!()
		}

	}
	pub fn codegen(
		&self,
		nc_table: &crate::lexer::NumericConstantTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
	) -> miette::Result<hirn::Expression> {
		use self::Expression::*;
		log::debug!("codegen expression: {:?}", self);
		match self {
			Number(num) => {
				let constant = nc_table.get_by_key(&num.key).unwrap(); //FIXME read additional information from local_ctx
				let signed = match constant.signed {
					Some(s) => s,
					None => false,
				};
				if signed{
					return Ok(
						hirn::Expression::Constant(hirn::design::NumericConstant::new_signed(constant.value.clone()))
					);
    			}
				Ok(
					hirn::Expression::Constant(hirn::design::NumericConstant::new_unsigned(constant.value.clone()))
				)
			},
			Identifier(id) => {
				let signal_id = scope.get_api_id(scope_id, &id.id).unwrap();
				Ok(signal_id.into())
			},
			ParenthesizedExpression(expr) => expr.expression.codegen(nc_table, scope_id, scope),
			MatchExpression(_) => todo!(),
			ConditionalExpression(_) => todo!(),
			Tuple(_) => unreachable!(),
			TernaryExpression(ternary) => {
				let cond = ternary.condition.codegen(nc_table, scope_id, scope)?;
				let true_branch = ternary.true_branch.codegen(nc_table, scope_id, scope)?;
				let false_branch = ternary.false_branch.codegen(nc_table, scope_id, scope)?;
				todo!("codegen ternary")
			},
			PostfixWithIndex(ind) => {
				let index = ind.index.codegen(nc_table, scope_id, scope)?;
				let mut expr = ind.expression.get_slice(nc_table, scope_id, scope)?;
				expr.indices.push(index);
				Ok(hirn::Expression::Signal(expr))
			},
			PostfixWithRange(_) => todo!(),
			PostfixWithArgs(builtin) => todo!(),
			PostfixWithId(_) => todo!(),
			UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				let operand = unary.expression.codegen(nc_table, scope_id, scope)?;
				match unary.code {
					LogicalNot => Ok(!operand),
					BitwiseNot => Ok(hirn::Expression::Unary(hirn::design::expression::UnaryExpression {
						op: hirn::design::expression::UnaryOp::BitwiseNot,
						operand: Box::new(operand),
					})),
					Minus => Ok(-operand),
					Plus => Ok(operand),
				}
			},
			UnaryCastExpression(_) => todo!(),
			BinaryExpression(binop) => {
				use crate::parser::ast::BinaryOpcode::*;
				let lhs = binop.lhs.codegen(nc_table, scope_id, scope)?;
				let rhs = binop.rhs.codegen(nc_table, scope_id, scope)?;
				match binop.code {
					Multiplication => Ok(lhs * rhs),
					Division => Ok(lhs / rhs),
					Addition => Ok(lhs + rhs),
					Subtraction => Ok(lhs - rhs),
					Modulo => Ok(lhs % rhs),
					Equal => Ok(hirn::Expression::Binary(hirn::design::expression::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::expression::BinaryOp::Equal,
						rhs: Box::new(rhs),
					})),
					NotEqual => Ok(hirn::Expression::Binary(hirn::design::expression::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::expression::BinaryOp::NotEqual,
						rhs: Box::new(rhs),
					})),
					LShift => Ok(lhs << rhs),
					RShift => Ok(lhs >> rhs),
					BitwiseAnd => Ok(lhs & rhs),
					BitwiseOr => Ok(lhs | rhs),
					BitwiseXor => Ok(lhs ^ rhs),
					Less => Ok(hirn::Expression::Binary(hirn::design::expression::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::expression::BinaryOp::Less,
						rhs: Box::new(rhs),
					})),
					Greater => Ok(hirn::Expression::Binary(hirn::design::expression::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::expression::BinaryOp::Greater,
						rhs: Box::new(rhs),
					})),
					LessEqual => Ok(hirn::Expression::Binary(hirn::design::expression::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::expression::BinaryOp::LessEqual,
						rhs: Box::new(rhs),
					})),
					GreaterEqual => Ok(hirn::Expression::Binary(hirn::design::expression::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::expression::BinaryOp::GreaterEqual,
						rhs: Box::new(rhs),
					})),
					LogicalAnd => Ok(hirn::Expression::Binary(hirn::design::expression::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::expression::BinaryOp::LogicalAnd,
						rhs: Box::new(rhs),
					})),
					LogicalOr => Ok(hirn::Expression::Binary(hirn::design::expression::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::expression::BinaryOp::LogicalOr,
						rhs: Box::new(rhs),
					})),
				}
			},
		}
	}
	pub fn evaluate_type(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		scope_id: usize,
		local_ctx: &mut LocalAnalyzerContex,
		coupling_type: Signal,
		is_lhs: bool,
		location: SourceSpan, 
	) -> miette::Result<Signal> {
		use Expression::*;
		match self {
			Number(num) => {
				// if coupling type is none, width must be known
				report_not_allowed_lhs(is_lhs, self.get_location())?;
				let width = coupling_type.width();
				let key = &num.key;
				let constant = global_ctx.nc_table.get_by_key(key).unwrap();
				let sig = Signal::new_from_constant(constant, num.location);
				match (width, sig.width()) {
					(None, None) => {
						return Err(miette::Report::new(
							SemanticError::WidthNotKnown
								.to_diagnostic_builder()
								.label(num.location, "Width of this expression is not known, but it should be")
								.label(location, "Because of this operation")
								.build(),
						))
					},
					(None, Some(_)) => {
						//local_ctx.nc_widths.insert(num.key, v) //FIXME
					},
					(Some(_), None) => (),
					(Some(coming), Some(original)) => {
						if coming != original {
							return Err(miette::Report::new(
								SemanticError::WidthMismatch
									.to_diagnostic_builder()
									.label(num.location, "Width of this expression does not match")
									.label(location, "Width mismach in this assignment")
									.label(coupling_type.get_width_location().unwrap(), "Width of this signal")
									.build(),
							));
						}
					},
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
				}.clone();
				let mut sig = var.var.kind.to_signal();
				if is_lhs {
					if coupling_type.sensitivity == SignalSensitivity::NoSensitivity {
						// register the variable
						//local_ctx.scope.add_coupling(from, identifier.id, scope_id)
					}
					sig.sensitivity.can_drive(&coupling_type.sensitivity, location, global_ctx)?;
				}
				use crate::analyzer::SignalType::*;
				sig.signal_type = 
				match (&sig.signal_type, &coupling_type.signal_type) {
					(Auto(_), Auto(_)) => sig.signal_type.clone(),
        			(Auto(_), _) => coupling_type.signal_type.clone(),
        			(Bus(bus), Bus(bus1)) => {
						use crate::analyzer::SignalSignedness::*;
						let mut new: crate::analyzer::BusType = bus1.clone();
						new.signedness = match (&bus.signedness, &bus1.signedness) {
        					(Signed(_), Signed(_)) | (Unsigned(_), Unsigned(_)) => bus.signedness.clone(),
        					(Signed(_), Unsigned(_)) => todo!(),
        					(_, NoSignedness) => bus.signedness.clone(),
        					(Unsigned(_), Signed(_)) => todo!(),
							(NoSignedness, _) => bus1.signedness.clone(),
    					};
						new.width = match (&bus.width, &bus1.width) {
        					(_, None) => bus.width.clone(),
        					(None, Some(_)) => bus1.width.clone(),
        					(Some(coming), Some(original)) => {
								match (&coming.get_value(), &original.get_value()) {
            						(Some(val1), Some(val2)) => {
										if val1 != val2 {
											return Err(miette::Report::new(
												SemanticError::DifferingBusWidths
													.to_diagnostic_builder()
													.label(
														location,
														format!(
															"Cannot assign signals - width mismatch. {} bits vs {} bits",
															val2, val1
														)
														.as_str(),
													)
													.label(bus1.location, "First width specified here")
													.label(bus.location, "Second width specified here")
													.build(),
											));
										}
									},
									_ => (),
        						}
								
								bus.width.clone()
							},
    					};
						SignalType::Bus(new)
					},
        			(Bus(bus), Wire(wire)) => return Err(miette::Report::new(
						SemanticError::BoundingWireWithBus
							.to_diagnostic_builder()
							.label(location, "Cannot assign bus to a wire and vice versa")
							.label(*wire, "Signal specified as wire here")
							.label(bus.location, "Signal specified as a bus here")
							.build(),
					)),
        			(Bus(_), Auto(_)) => sig.signal_type.clone(),
        			(Wire(wire), Bus(bus)) => return Err(miette::Report::new(
						SemanticError::BoundingWireWithBus
							.to_diagnostic_builder()
							.label(location, "Cannot assign bus to a wire and vice versa")
							.label(*wire, "Signal specified as wire here")
							.label(bus.location, "Signal specified as a bus here")
							.build(),
					)),
        			(Wire(_), _) => sig.signal_type.clone(),
    			};
				if var.var.kind == crate::analyzer::VariableKind::Signal(sig.clone()) {
					return Ok(sig);
				}
				var.var.kind = crate::analyzer::VariableKind::Signal(sig.clone());
				local_ctx.scope.redeclare_variable(var);
				Ok(sig)
			},
			ParenthesizedExpression(expr) => {
				expr.expression
					.evaluate_type(global_ctx, scope_id,  local_ctx, coupling_type, is_lhs, location)
			},
			MatchExpression(_) => {
				report_not_allowed_lhs(is_lhs, self.get_location())?;
				todo!()
			},
			ConditionalExpression(_) => {
				report_not_allowed_lhs(is_lhs, self.get_location())?;
				todo!()
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
				report_not_allowed_lhs(is_lhs, self.get_location())?;
				let type_first = ternary
					.true_branch
					.evaluate_type(global_ctx, scope_id,  local_ctx, Signal::new_empty(), is_lhs, location)?;
				let type_second = ternary.false_branch.evaluate_type( 
					global_ctx,
					scope_id,
					 local_ctx,
					coupling_type.clone(),
					is_lhs,
					location,
				)?;
				use SignalType::*;
				match (&type_first.signal_type, &type_second.signal_type) {
        			(Bus(_), Bus(_)) => todo!(),
        			(Bus(_), Wire(_)) => todo!(),
        			(Bus(_), Auto(_)) => todo!(),
        			(Wire(_), Bus(_)) => todo!(),
        			(Wire(_), Wire(_)) => todo!(),
        			(Wire(_), Auto(_)) => todo!(),
        			(Auto(_), Bus(_)) => todo!(),
        			(Auto(_), Wire(_)) => todo!(),
        			(Auto(_), Auto(_)) => todo!(),
    			}
				let type_condition =
					ternary
						.condition
						.evaluate_type(global_ctx, scope_id,  local_ctx, coupling_type, is_lhs, location)?;
				Ok(type_first) // FIXME
			},
			PostfixWithIndex(index) => {
				//let mut expr = index.expression.evaluate_type(global_ctx, scope_id, local_ctx, coupling_type, is_lhs, location)?;
				//if !expr.is_array() && expr.is_bus() {
				//	return Err(miette::Report::new(SemanticError::ExpressionNonIndexable.to_diagnostic_builder()
				//		.label(index.location, "This expression cannot be indexed")
				//		.build()))
				//}
				//let ind = index.index.evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?;
				//if expr.is_array() {
				//	if ind.value < BigInt::from(0) && ind.value >= expr.dimensions.last().unwrap().get_value().clone().unwrap() {
				//		return Err(miette::Report::new(SemanticError::IndexOutOfBounds.to_diagnostic_builder()
				//			.label(index.location, "Index is out of bounds")
				//			.build()))
				//	}
				//	expr.dimensions.pop();
				//	return Ok(expr)
				//}
				//if let SignalType::Bus(bus) = &expr.signal_type {
				//	match &bus.width.clone().unwrap().get_value(){
        		//		Some(val) => {
				//			if ind.value < BigInt::from(0) && &ind.value >= val {
				//				return Err(miette::Report::new(SemanticError::IndexOutOfBounds.to_diagnostic_builder()
				//					.label(index.location, "Index is out of bounds")
				//					.build()))
				//			}
				//		},
        		//		None => (),
    			//	}
					
				//	expr.set_width(BusWidth::Evaluated(ind.value), bus.signedness.clone(), index.location);
				//	return Ok(expr)
				//}
				unreachable!()
			},
			PostfixWithRange(range) => {
				todo!();
				//let mut expr = range.expression.evaluate_type(global_ctx, scope_id,  local_ctx, coupling_type.clone(), is_lhs, location)?;
				//if expr.dimensions.len() > 0 {
				//	return Err(miette::Report::new(
				//		SemanticError::RangeOnNonBus
				//			.to_diagnostic_builder()
				//			//.label(range.location, "Range can only be applied to a bus")
				//			.label(	range.expression.get_location(), "This signal is an array, it cannot be range indexed")
				//			.build(),
				//	));
				//}
				//use SignalType::*;
				//match &expr.signal_type {
        		//	Bus(bus) => {
				//		let begin = range.range.lhs.evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?;
				//		let mut end = range.range.rhs.evaluate(global_ctx.nc_table, scope_id, &local_ctx.scope)?;
				//		use crate::parser::ast::RangeOpcode::*;
				//		match range.range.code{
        		//			Colon => (),
        		//			PlusColon => end = NumericConstant::new_from_binary(end.clone(), begin.clone(), |e1, e2| e1 + e2),
        		//			ColonLessThan => (), // FIXME
    			//		}
				//		match &bus.width{
        		//			Some(val) => {
				//				//match &val.get_value() {
            	//				//	Some(value) => {
				//				//		if &begin.value > value || &end.value > value {
				//				//			return Err(miette::Report::new(
				//				//				SemanticError::WidthMismatch.to_diagnostic_builder()
				//				//					.label(range.location, "Cannot perform range indexing - range is out of bounds")
				//				//					.label(range.location, format!("Range bounds are: {}:{} but actual width is: {:?}", begin.value, end.value, val).as_str())
				//				//					.build(),
				//				//			))
				//				//		}
				//				//	},
            	//				//	None => (),
        		//				//}
								
				//				//local_ctx.scope.evaluated_expressions.insert(range.location, self.clone());
				//				//expr.set_width(crate::analyzer::BusWidth::EvaluatedLocated(end.value - begin.value, range.location), bus.signedness.clone(), range.location);
				//				//Ok(expr)
				//			},
        		//			None => return Err(miette::Report::new(
				//				SemanticError::WidthNotKnown.to_diagnostic_builder()
				//					.label(range.location, "Width of this expression is not known, but it should be")
				//					.build())),
    			//		}
				//	},
        		//	Wire(type_loc) | Auto(type_loc) => {
				//		return Err(miette::Report::new(
				//			SemanticError::RangeOnNonBus
				//				.to_diagnostic_builder()
				//				.label(range.location, "Range can only be applied to a bus")
				//				.label(*type_loc, "This signal is not a bus")
				//				.build(),
				//		));
				//	}
    			//}				
			},
			PostfixWithArgs(_) => {
				report_not_allowed_lhs(is_lhs, self.get_location())?;
				todo!()
			},
			PostfixWithId(module) => {
				let m = local_ctx.scope.get_variable(scope_id, &module.expression);
				match m {
					Some(var) => match &var.var.kind {
						crate::analyzer::VariableKind::ModuleInstance(module) => {
							todo!()
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
			UnaryOperatorExpression(_) => {
				report_not_allowed_lhs(is_lhs, self.get_location())?;
				todo!()
			},
			UnaryCastExpression(_) => {
				report_not_allowed_lhs(is_lhs, self.get_location())?;
				todo!()
			},
			BinaryExpression(binop) => {
				report_not_allowed_lhs(is_lhs, self.get_location())?;
				use crate::parser::ast::BinaryOpcode::*;
				let type_first =
					binop
						.lhs
						.evaluate_type(global_ctx, scope_id,  local_ctx, Signal::new_empty(), is_lhs, binop.location)?;
				if ! type_first.is_width_specified() {
					return Err(miette::Report::new(
						SemanticError::WidthNotKnown
							.to_diagnostic_builder()
							.label(binop.location, "Width of this expression is not known, but it should be")
							.label(location, "Cannot perform addition, width is not known")
							.build(),
					))
				}
				let type_second =
					binop
						.rhs
						.evaluate_type(global_ctx, scope_id,  local_ctx, Signal::new_empty(), is_lhs, binop.location)?;
				if ! type_second.is_width_specified() {
					return Err(miette::Report::new(
						SemanticError::WidthNotKnown
							.to_diagnostic_builder()
							.label(binop.location, "Width of this expression is not known, but it should be")
							.label(location, "Cannot perform addition, width is not known")
							.build(),
					))
				}

				let new_width = match &binop.code {
					Multiplication => {},
					Division => todo!(),
					Addition => {
						use SignalType::*;
						match (&type_first.signal_type, &type_second.signal_type) {
        					(Bus(_), Bus(_)) => todo!(),
        					(Bus(_), Wire(_)) => todo!(),
        					(Bus(_), Auto(_)) => todo!(),
        					(Wire(_), Bus(_)) => todo!(),
        					(Wire(_), Wire(_)) => todo!(),
        					(Wire(_), Auto(_)) => todo!(),
        					(Auto(_), Bus(_)) => todo!(),
        					(Auto(_), Wire(_)) => todo!(),
        					(Auto(_), Auto(_)) => todo!(),
    					}
					},
					Subtraction => todo!(),
					Modulo => todo!(),
					Equal => todo!(),
					NotEqual => todo!(),
					LShift => todo!(),
					RShift => todo!(),
					BitwiseAnd => todo!(),
					BitwiseOr => todo!(),
					BitwiseXor => todo!(),
					Less => todo!(),
					Greater => todo!(),
					LessEqual => todo!(),
					GreaterEqual => todo!(),
					LogicalAnd => todo!(),
					LogicalOr => todo!(),
				};
				todo!()
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
fn report_not_allowed_lhs(is_lhs: bool, location: SourceSpan) -> miette::Result<()> {
	if is_lhs {
		return Err(miette::Report::new(
			SemanticError::ForbiddenExpressionInLhs
				.to_diagnostic_builder()
				.label(
					location,
					"This expression is not allowed in the left hand sight of assignment",
				)
				.build(),
		));
	}
	Ok(())
}
