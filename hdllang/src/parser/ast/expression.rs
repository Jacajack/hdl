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
use crate::analyzer::SemanticError;
use crate::lexer::{IdTableKey, NumericConstantBase};
use crate::parser::ast::{opcodes::*, MatchExpressionStatement, RangeExpression, SourceLocation, TypeName};
use crate::{ProvidesCompilerDiagnostic, SourceSpan};
pub use binary_expression::BinaryExpression;
pub use conditional_expression::ConditionalExpression;
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
pub use ternary_expression::TernaryExpression;
pub use tuple::Tuple;
pub use unary_cast_expression::UnaryCastExpression;
pub use unary_operator_expression::UnaryOperatorExpression;

use num_bigint::{BigInt, Sign};
use crate::lexer::NumericConstant;
#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub enum Expression {
	Number(Number),
	Identifier(Identifier),
	ParenthesizedExpression(ParenthesizedExpression),
	MatchExpression(MatchExpression),
	ConditionalExpression(ConditionalExpression),
	Tuple(Tuple),
	TernaryExpression(TernaryExpression),
	PostfixWithIndex(PostfixWithIndex),
	PostfixWithRange(PostfixWithRange),
	PostfixWithArgs(PostfixWithArgs),
	PostfixWithId(PostfixWithId),
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
	pub fn evaluate_in_declaration(&self, nc_table: &crate::lexer::NumericConstantTable) -> miette::Result<NumericConstant> {
		match self {
			Expression::Number(nc_key) => {
				let constant = nc_table.get_by_key(&nc_key.key).unwrap();
				//if let Some(signed) = constant.signed {
				//	if signed {
				//		return Ok(signed_big_uint_to_big_int(&constant.value));
				//	}
				//
				Ok(constant.clone())
			},
			Expression::Identifier(id) => {
				return Err(miette::Report::new(
					SemanticError::NonGenericTypeVariableInExpression
						.to_diagnostic_builder()
						.label(
							id.location,
							"This variable is used in expression but its value its not known at compile time",
						)
						.build(),
				))
			},
			Expression::ParenthesizedExpression(expr) => expr.expression.evaluate_in_declaration(nc_table),
			Expression::MatchExpression(_) => todo!(),
			Expression::ConditionalExpression(_) => todo!(),
			Expression::Tuple(_) => unreachable!(),
			Expression::TernaryExpression(tern) => Ok(
				if tern.condition.evaluate_in_declaration(nc_table)?.value != BigInt::from(0) {
					tern.true_branch.evaluate_in_declaration(nc_table)?
				} else {
					tern.false_branch.evaluate_in_declaration(nc_table)?
				},
			),
			Expression::PostfixWithIndex(_) => todo!(), // nie bedzie
			Expression::PostfixWithRange(_) => todo!(), // nie bedzie
			Expression::PostfixWithArgs(_) => todo!(),  // szerokość busa
			Expression::PostfixWithId(_) => todo!(),    // nie bedzie
			Expression::UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					LogicalNot => Ok(
						if unary.expression.evaluate_in_declaration(nc_table)?.value == BigInt::from(0) {
							NumericConstant::new(BigInt::from(1), None, None, Some(NumericConstantBase::Boolean))
						} else {
							NumericConstant::new(BigInt::from(0), None, None, Some(NumericConstantBase::Boolean))
						},
					),
					BitwiseNot => Ok(NumericConstant::new_from_unary(unary.expression.evaluate_in_declaration(nc_table)?,|e| !e)),
					Minus => { 
						let other = unary.expression.evaluate_in_declaration(nc_table)?;
						if let Some(false) = other.signed{
							// report an error
						}
						Ok(NumericConstant::new_from_unary(other,|e| -e))
					}, // catch unsigned
					Plus => Ok(NumericConstant::new_from_unary(unary.expression.evaluate_in_declaration(nc_table)?,|e| e*(-1))),
				}
			},
			Expression::UnaryCastExpression(_) => todo!(), // nie bedzie
			Expression::BinaryExpression(binop) => {
				use crate::parser::ast::BinaryOpcode::*;
				match binop.code {
					Multiplication => {
						Ok(NumericConstant::new_from_binary(
							binop.lhs.evaluate_in_declaration(nc_table)?,
							binop.rhs.evaluate_in_declaration(nc_table)?,
							|e1, e2| e1 * e2,
						))
					},
					Division => {
						let rhs = binop.rhs.evaluate_in_declaration(nc_table)?;
						if rhs.value.is_zero() {
							return Err(miette::Report::new(
								SemanticError::DivisionByZero
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Division by zero is not allowed")
									.build(),
							));
						}
						Ok(NumericConstant::new_from_binary(binop.lhs.evaluate_in_declaration(nc_table)?, rhs, |e1, e2| e1 / e2))
					},
					Addition => {
						Ok(NumericConstant::new_from_binary(
							binop.lhs.evaluate_in_declaration(nc_table)?,
							binop.rhs.evaluate_in_declaration(nc_table)?,
							|e1, e2| e1 + e2,
						))
					},
					Subtraction => {
						Ok(NumericConstant::new_from_binary(
							binop.lhs.evaluate_in_declaration(nc_table)?,
							binop.rhs.evaluate_in_declaration(nc_table)?,
							|e1, e2| e1 - e2,
						))
					},
					Modulo => {
						let rhs = binop.rhs.evaluate_in_declaration(nc_table)?;
						if rhs.value.is_zero() {
							return Err(miette::Report::new(
								SemanticError::DivisionByZero
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "It is not allowed to modulo by zero")
									.build(),
							));
						}
						Ok(NumericConstant::new_from_binary(binop.lhs.evaluate_in_declaration(nc_table)?, rhs, |e1, e2| e1 / e2))
					},
					Equal => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?.value
							== binop.rhs.evaluate_in_declaration(nc_table)?.value
						{
							NumericConstant::from_u64(
								1,
								Some(1),
								Some(false),
								Some(NumericConstantBase::Boolean))
						} else {
							NumericConstant::from_u64(
								0,
								Some(1),
								Some(false),
								Some(NumericConstantBase::Boolean))
						},
					),
					NotEqual => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?.value
							!= binop.rhs.evaluate_in_declaration(nc_table)?.value
						{
							NumericConstant::from_u64(
								1,
								Some(1),
								Some(false),
								Some(NumericConstantBase::Boolean))
						} else {
							NumericConstant::from_u64(
								0,
								Some(1),
								Some(false),
								Some(NumericConstantBase::Boolean))
						},
					),
					LShift => {
						let mut lhs = binop.lhs.evaluate_in_declaration(nc_table)?;
						let rhs = binop.rhs.evaluate_in_declaration(nc_table)?;
						if rhs.value.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						} else {
							let mut i = BigInt::from(0);
							while i < rhs.value {
								lhs.value = lhs.value << 1;
								i += BigInt::from(1);
							}
							Ok(lhs)
						}
					},
					RShift => {
						let mut lhs = binop.lhs.evaluate_in_declaration(nc_table)?;
						let rhs = binop.rhs.evaluate_in_declaration(nc_table)?;
						if rhs.value.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						} else {
							let mut i = BigInt::from(0);
							while i < rhs.value {
								lhs.value = lhs.value >> 1;
								i += BigInt::from(1);
							}
							Ok(lhs)
						}
					},
					BitwiseAnd => {
						Ok(NumericConstant::new_from_binary(
							binop.lhs.evaluate_in_declaration(nc_table)?,
							binop.rhs.evaluate_in_declaration(nc_table)?,
							|e1, e2| e1 & e2,
						))
					},
					BitwiseOr => {
						Ok(NumericConstant::new_from_binary(
							binop.lhs.evaluate_in_declaration(nc_table)?,
							binop.rhs.evaluate_in_declaration(nc_table)?,
							|e1, e2| e1 | e2,
						))
					},
					BitwiseXor => {
						Ok(NumericConstant::new_from_binary(
							binop.lhs.evaluate_in_declaration(nc_table)?,
							binop.rhs.evaluate_in_declaration(nc_table)?,
							|e1, e2| e1 ^ e2,
						))
					},
					Less => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?.value < binop.rhs.evaluate_in_declaration(nc_table)?.value {
							NumericConstant::new_true()
						} else {
							NumericConstant::new_false()
						},
					),
					Greater => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?.value > binop.rhs.evaluate_in_declaration(nc_table)?.value {
							NumericConstant::new_true()
						} else {
							NumericConstant::new_false()
						},
					),
					LessEqual => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?.value
							<= binop.rhs.evaluate_in_declaration(nc_table)?.value
						{
							NumericConstant::new_true()
						} else {
							NumericConstant::new_false()
						},
					),
					GreaterEqual => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?.value
							>= binop.rhs.evaluate_in_declaration(nc_table)?.value
						{
							NumericConstant::new_true()
						} else {
							NumericConstant::new_false()
						},
					),
					LogicalAnd => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?.value != BigInt::from(0)
							&& binop.rhs.evaluate_in_declaration(nc_table)?.value != BigInt::from(0)
						{
							NumericConstant::new_true()
						} else {
							NumericConstant::new_false()
						},
					),
					LogicalOr => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?.value != BigInt::from(0)
							|| binop.rhs.evaluate_in_declaration(nc_table)?.value != BigInt::from(0)
						{
							NumericConstant::new_true()
						} else {
							NumericConstant::new_false()
						},
					),
				}
			},
		}
	}
}

