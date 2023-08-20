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
use crate::lexer::IdTableKey;
use crate::parser::ast::{opcodes::*, MatchExpressionStatement, RangeExpression, SourceLocation, TypeName};
use crate::{ProvidesCompilerDiagnostic, SourceSpan};
pub use binary_expression::BinaryExpression;
pub use conditional_expression::ConditionalExpression;
pub use identifier::Identifier;
use log::debug;
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
	pub fn evaluate_in_declaration(&self, nc_table: &crate::lexer::NumericConstantTable) -> miette::Result<BigInt> {
		match self {
			Expression::Number(nc_key) => {
				let constant = nc_table.get_by_key(&nc_key.key).unwrap();
				//if let Some(signed) = constant.signed {
				//	if signed {
				//		return Ok(signed_big_uint_to_big_int(&constant.value));
				//	}
				//
				Ok(constant.value.clone())
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
				if tern.condition.evaluate_in_declaration(nc_table)? != BigInt::from(0) {
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
						if unary.expression.evaluate_in_declaration(nc_table)? == BigInt::from(0) {
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
					BitwiseNot => Ok(!unary.expression.evaluate_in_declaration(nc_table)?),
					Minus => Ok(-unary.expression.evaluate_in_declaration(nc_table)?), // catch unsigned
					Plus => Ok(unary.expression.evaluate_in_declaration(nc_table)?),
				}
			},
			Expression::UnaryCastExpression(_) => todo!(), // nie bedzie
			Expression::BinaryExpression(binop) => {
				use crate::parser::ast::BinaryOpcode::*;
				match binop.code {
					Multiplication => {
						Ok(binop.lhs.evaluate_in_declaration(nc_table)?
							* binop.rhs.evaluate_in_declaration(nc_table)?)
					},
					Division => {
						let rhs = binop.rhs.evaluate_in_declaration(nc_table)?;
						if rhs.is_zero() {
							return Err(miette::Report::new(
								SemanticError::DivisionByZero
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Division by zero is not allowed")
									.build(),
							));
						}
						Ok(binop.lhs.evaluate_in_declaration(nc_table)? / rhs)
					},
					Addition => {
						Ok(binop.lhs.evaluate_in_declaration(nc_table)?
							+ binop.rhs.evaluate_in_declaration(nc_table)?)
					},
					Subtraction => {
						Ok(binop.lhs.evaluate_in_declaration(nc_table)?
							- binop.rhs.evaluate_in_declaration(nc_table)?)
					},
					Modulo => {
						let rhs = binop.rhs.evaluate_in_declaration(nc_table)?;
						if rhs.is_zero() {
							return Err(miette::Report::new(
								SemanticError::DivisionByZero
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "It is not allowed to modulo by zero")
									.build(),
							));
						}
						Ok(binop.lhs.evaluate_in_declaration(nc_table)? % rhs)
					},
					Equal => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?
							== binop.rhs.evaluate_in_declaration(nc_table)?
						{
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
					NotEqual => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?
							!= binop.rhs.evaluate_in_declaration(nc_table)?
						{
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
					LShift => {
						let mut lhs = binop.lhs.evaluate_in_declaration(nc_table)?;
						let rhs = binop.rhs.evaluate_in_declaration(nc_table)?;
						if rhs.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						} else {
							let mut i = BigInt::from(0);
							while i < rhs {
								lhs = lhs << 1;
								i += BigInt::from(1);
							}
							Ok(lhs)
						}
					},
					RShift => {
						let mut lhs = binop.lhs.evaluate_in_declaration(nc_table)?;
						let rhs = binop.rhs.evaluate_in_declaration(nc_table)?;
						if rhs.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						} else {
							let mut i = BigInt::from(0);
							while i < rhs {
								lhs = lhs >> 1;
								i += BigInt::from(1);
							}
							Ok(lhs)
						}
					},
					BitwiseAnd => {
						Ok(binop.lhs.evaluate_in_declaration(nc_table)?
							& binop.rhs.evaluate_in_declaration(nc_table)?)
					},
					BitwiseOr => {
						Ok(binop.lhs.evaluate_in_declaration(nc_table)?
							| binop.rhs.evaluate_in_declaration(nc_table)?)
					},
					BitwiseXor => {
						Ok(binop.lhs.evaluate_in_declaration(nc_table)?
							^ binop.rhs.evaluate_in_declaration(nc_table)?)
					},
					Less => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)? < binop.rhs.evaluate_in_declaration(nc_table)? {
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
					Greater => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)? > binop.rhs.evaluate_in_declaration(nc_table)? {
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
					LessEqual => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?
							<= binop.rhs.evaluate_in_declaration(nc_table)?
						{
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
					GreaterEqual => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)?
							>= binop.rhs.evaluate_in_declaration(nc_table)?
						{
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
					LogicalAnd => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)? != BigInt::from(0)
							&& binop.rhs.evaluate_in_declaration(nc_table)? != BigInt::from(0)
						{
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
					LogicalOr => Ok(
						if binop.lhs.evaluate_in_declaration(nc_table)? != BigInt::from(0)
							|| binop.rhs.evaluate_in_declaration(nc_table)? != BigInt::from(0)
						{
							BigInt::from(1)
						} else {
							BigInt::from(0)
						},
					),
				}
			},
		}
	}
}

