use crate::core::numeric_constant_table::NumericConstantTableKey;
use crate::parser::ast::{opcodes::*, MatchExpressionStatement, SourceLocation, TypeName};
use crate::{lexer::IdTableKey, SourceSpan};
use std::fmt::{Debug, Error, Formatter};

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
		expressions: Vec<Box<Expression>>,
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
		range: Box<Expression>,
		location: SourceSpan,
	},
	PostfixWithArgs {
		expression: Box<Expression>,
		argument_list: Vec<Box<Expression>>,
		location: SourceSpan,
	},
	PostfixEmptyCall {
		expression: Box<Expression>,
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
	RangeExpression {
		lhs: Box<Expression>,
		rhs: Box<Expression>,
		code: RangeOpcode,
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
			Number { key: value, location: _ } => write!(fmt, "{:?}", value),
			Identifier { id: _, location: _ } => write!(fmt, "foo"),
			ParenthesizedExpression {
				expression,
				location: _,
			} => write!(fmt, "({:?})", *expression),
			BinaryExpression {
				lhs,
				rhs,
				code,
				location: _,
			} => {
				write!(fmt, "({:?} {:?} {:?})", lhs, code, rhs)
			},
			TernaryExpression {
				condition,
				true_branch,
				false_branch,
				location: _,
			} => {
				write!(fmt, "({:?} ? {:?} : {:?})", condition, true_branch, false_branch)
			},
			RangeExpression {
				lhs,
				rhs,
				location: _,
				code,
			} => {
				write!(fmt, "([{:?}{:?}{:?}])", lhs, code, rhs)
			},
			UnaryOperatorExpression {
				expression,
				code,
				location: _,
			} => write!(fmt, "{:?}{:?}", code, expression),
			PostfixWithId {
				expression,
				id,
				location: _,
			} => write!(fmt, "({:?}.{:?})", expression, id),
			PostfixWithIndex {
				expression,
				index,
				location: _,
			} => write!(fmt, "({:?}[{:?}])", expression, index),
			PostfixWithRange {
				expression,
				range,
				location: _,
			} => write!(fmt, "({:?}{:?})", expression, range),
			PostfixWithArgs {
				expression,
				argument_list,
				location: _,
			} => {
				write!(fmt, "({:?}(", expression)?;
				for arg in argument_list.into_iter() {
					write!(fmt, "{:?}, ", arg)?;
				}
				write!(fmt, "))")
			},
			PostfixEmptyCall {
				expression,
				location: _,
			} => write!(fmt, "({:?}())", expression),
			UnaryCastExpression {
				type_name,
				expression,
				location: _,
			} => write!(fmt, "(({:?}){:?})", type_name, expression),
			Tuple {
				expressions,
				location: _,
			} => {
				write!(fmt, "{{")?;
				for expr in expressions.into_iter() {
					write!(fmt, "{:?},", expr)?;
				}
				write!(fmt, "}}")
			},
			MatchExpression {
				value,
				statements,
				location: _,
			} => {
				write!(fmt, "match({:?}){{\n", value)?;
				for s in statements.into_iter() {
					write!(fmt, "{:?},\n", s)?;
				}
				write!(fmt, "}}")
			},
			ConditionalExpression {
				statements,
				location: _,
			} => {
				write!(fmt, "conditional{{\n")?;
				for s in statements.into_iter() {
					write!(fmt, "{:?},\n", s)?;
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
			PostfixEmptyCall { location, .. } => location,
			PostfixWithId { location, .. } => location,
			UnaryOperatorExpression { location, .. } => location,
			UnaryCastExpression { location, .. } => location,
			RangeExpression { location, .. } => location,
			BinaryExpression { location, .. } => location,
			Error => todo!(),
		}
	}
}
