mod pretty_printable;

use crate::core::numeric_constant_table::NumericConstantTableKey;
use crate::parser::ast::{opcodes::*, MatchExpressionStatement, RangeExpression, SourceLocation, TypeName};
use crate::{lexer::IdTableKey, SourceSpan};
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Error, Formatter};

#[derive(Serialize, Deserialize, Clone)]
pub enum Expression {
	Number {
		key: NumericConstantTableKey,
		location: SourceSpan,
	},
	Identifier {
		id: IdTableKey,
		location: SourceSpan,
	},
	ParenthesizedExpression {
		expression: Box<Expression>,
		location: SourceSpan,
	},
	MatchExpression {
		value: Box<Expression>,
		statements: Vec<MatchExpressionStatement>,
		location: SourceSpan,
	},
	ConditionalExpression {
		statements: Vec<MatchExpressionStatement>,
		location: SourceSpan,
	},
	Tuple {
		expressions: Vec<Expression>,
		location: SourceSpan,
	},
	TernaryExpression {
		condition: Box<Expression>,
		true_branch: Box<Expression>,
		false_branch: Box<Expression>,
		location: SourceSpan,
	},
	PostfixWithIndex {
		expression: Box<Expression>,
		index: Box<Expression>,
		location: SourceSpan,
	},
	PostfixWithRange {
		expression: Box<Expression>,
		range: RangeExpression,
		location: SourceSpan,
	},
	PostfixWithArgs {
		expression: Box<Expression>,
		argument_list: Vec<Expression>,
		location: SourceSpan,
	},
	PostfixWithId {
		expression: Box<Expression>,
		id: IdTableKey,
		location: SourceSpan,
	},
	UnaryOperatorExpression {
		expression: Box<Expression>,
		code: UnaryOpcode,
		location: SourceSpan,
	},
	UnaryCastExpression {
		type_name: TypeName,
		expression: Box<Expression>,
		location: SourceSpan,
	},
	BinaryExpression {
		lhs: Box<Expression>,
		rhs: Box<Expression>,
		code: BinaryOpcode,
		location: SourceSpan,
	},
	Error,
}

impl Debug for Expression {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::Expression::*;
		match &self {
			Number { key, .. } => write!(fmt, "{:?}", key),
			Identifier { .. } => write!(fmt, "foo"),
			ParenthesizedExpression { expression, .. } => write!(fmt, "({:?})", *expression),
			BinaryExpression { lhs, rhs, code, .. } => {
				write!(fmt, "({:?} {:?} {:?})", lhs, code, rhs)
			},
			TernaryExpression {
				condition,
				true_branch,
				false_branch,
				..
			} => {
				write!(fmt, "({:?} ? {:?} : {:?})", condition, true_branch, false_branch)
			},
			UnaryOperatorExpression { expression, code, .. } => write!(fmt, "{:?}{:?}", code, expression),
			PostfixWithId { expression, id, .. } => write!(fmt, "({:?}.{:?})", expression, id),
			PostfixWithIndex { expression, index, .. } => write!(fmt, "({:?}[{:?}])", expression, index),
			PostfixWithRange { expression, range, .. } => write!(fmt, "({:?}{:?})", expression, range),
			PostfixWithArgs {
				expression,
				argument_list,
				..
			} => {
				write!(fmt, "({:?}(", expression)?;
				for arg in argument_list.iter() {
					write!(fmt, "{:?}, ", arg)?;
				}
				write!(fmt, "))")
			},
			//PostfixEmptyCall { expression, .. } => write!(fmt, "({:?}())", expression),
			UnaryCastExpression {
				type_name, expression, ..
			} => write!(fmt, "(({:?}){:?})", type_name, expression),
			Tuple { expressions, .. } => {
				write!(fmt, "{{")?;
				for expr in expressions.iter() {
					write!(fmt, "{:?},", expr)?;
				}
				write!(fmt, "}}")
			},
			MatchExpression { value, statements, .. } => {
				writeln!(fmt, "match({:?}){{", value)?;
				for s in statements.iter() {
					writeln!(fmt, "{:?},", s)?;
				}
				write!(fmt, "}}")
			},
			ConditionalExpression { statements, .. } => {
				writeln!(fmt, "conditional{{")?;
				for s in statements.iter() {
					writeln!(fmt, "{:?},", s)?;
				}
				write!(fmt, "}}")
			},
			Error => write!(fmt, "error"),
		}
	}
}
impl SourceLocation for Expression {
	fn get_location(&self) -> SourceSpan {
		use self::Expression::*;
		match *self {
			Number { location, .. } => location,
			Identifier { location, .. } => location,
			ParenthesizedExpression { location, .. } => location,
			MatchExpression { location, .. } => location,
			ConditionalExpression { location, .. } => location,
			Tuple { location, .. } => location,
			TernaryExpression { location, .. } => location,
			PostfixWithIndex { location, .. } => location,
			PostfixWithRange { location, .. } => location,
			PostfixWithArgs { location, .. } => location,
			//PostfixEmptyCall { location, .. } => location,
			PostfixWithId { location, .. } => location,
			UnaryOperatorExpression { location, .. } => location,
			UnaryCastExpression { location, .. } => location,
			//RangeExpression { location, .. } => location,
			BinaryExpression { location, .. } => location,
			Error => todo!(),
		}
	}
}
