mod pretty_printable;
mod number;
mod identifier;
mod parenthesized;
mod match_expression;
mod conditional_expression;
mod tuple;
mod ternary_expression;
mod postfix_with_index;
mod binary_expression;
mod postfix_with_args;
mod postfix_with_id;
mod unary_operator_expression;
mod unary_cast_expression;
mod postfix_with_range;

pub use postfix_with_id::PostfixWithId;
pub use unary_cast_expression::UnaryCastExpression;
pub use unary_operator_expression::UnaryOperatorExpression;
pub use postfix_with_range::PostfixWithRange;
pub use postfix_with_args::PostfixWithArgs;
pub use binary_expression::BinaryExpression;
pub use postfix_with_index::PostfixWithIndex;
pub use ternary_expression::TernaryExpression;
pub use tuple::Tuple;
pub use conditional_expression::ConditionalExpression;
pub use match_expression::MatchExpression;
pub use parenthesized::ParenthesizedExpression;
pub use identifier::Identifier;
pub use number::Number;
use crate::parser::ast::{opcodes::*, MatchExpressionStatement, RangeExpression, SourceLocation, TypeName};
use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};

#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub enum Expression {
	Number (Number),
	Identifier (Identifier),
	ParenthesizedExpression (ParenthesizedExpression),
	MatchExpression (MatchExpression),
	ConditionalExpression (ConditionalExpression),
	Tuple(Tuple),
	TernaryExpression (TernaryExpression),
	PostfixWithIndex (PostfixWithIndex),
	PostfixWithRange (PostfixWithRange),
	PostfixWithArgs (PostfixWithArgs),
	PostfixWithId (PostfixWithId),
	UnaryOperatorExpression (UnaryOperatorExpression),
	UnaryCastExpression (UnaryCastExpression),
	BinaryExpression (BinaryExpression),
}

impl Debug for Expression {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::Expression::*;
		match &self {
			Number (number) => write!(fmt, "{:?}", number.key),
			Identifier (_) => write!(fmt, "foo"),
			ParenthesizedExpression (expr) => write!(fmt, "({:?})", *expr.expression),
			BinaryExpression (binop) => {
				write!(fmt, "({:?} {:?} {:?})", binop.lhs, binop.code, binop.rhs)
			},
			TernaryExpression (teranary) => {
				write!(fmt, "({:?} ? {:?} : {:?})", teranary.condition, teranary.true_branch, teranary.false_branch)
			},
			UnaryOperatorExpression (unary) => write!(fmt, "{:?}{:?}", unary.code, unary.expression),
			PostfixWithId (postfix) => write!(fmt, "({:?}.{:?})", postfix.expression, postfix.id),
			PostfixWithIndex (postfix) => write!(fmt, "({:?}[{:?}])", postfix.expression, postfix.index),
			PostfixWithRange (postfix) => write!(fmt, "({:?}{:?})", postfix.expression, postfix.range),
			PostfixWithArgs (postfix) => {
				write!(fmt, "({:?}(", postfix.expression)?;
				for arg in postfix.argument_list.iter() {
					write!(fmt, "{:?}, ", arg)?;
				}
				write!(fmt, "))")
			},
			UnaryCastExpression (unary) => write!(fmt, "(({:?}){:?})", unary.type_name, unary.expression),
			Tuple (tuple) => {
				write!(fmt, "{{")?;
				for expr in tuple.expressions.iter() {
					write!(fmt, "{:?},", expr)?;
				}
				write!(fmt, "}}")
			},
			MatchExpression (match_expr) => {
				writeln!(fmt, "match({:?}){{", match_expr.value)?;
				for s in match_expr.statements.iter() {
					writeln!(fmt, "{:?},", s)?;
				}
				write!(fmt, "}}")
			},
			ConditionalExpression (cond) => {
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
			Number (number) => number.location,
			Identifier (ident) => ident.location,
			ParenthesizedExpression (expr) => expr.location,
			MatchExpression (match_expr) => match_expr.location,
			ConditionalExpression (cond) => cond.location,
			Tuple (tuple) => tuple.location,
			TernaryExpression (ternary) => ternary.location,
			PostfixWithIndex (postfix) => postfix.location,
			PostfixWithRange (postfix) => postfix.location,
			PostfixWithArgs (postfix) => postfix.location,
			PostfixWithId (postfix) => postfix.location,
			UnaryOperatorExpression (unary) => unary.location,
			UnaryCastExpression (unary) => unary.location,
			BinaryExpression (binop) => binop.location,
		}
	}
}
