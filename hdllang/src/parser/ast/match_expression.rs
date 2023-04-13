use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};

use crate::parser::ast::{Expression, SourceLocation};

pub struct MatchExpressionStatement {
	pub antecedent: MatchExpressionAntecendent,
	pub expression: Box<Expression>,
	pub location: SourceSpan,
}

impl Debug for MatchExpressionStatement {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		write!(fmt, "{:?} => {:?}", self.antecedent, self.expression)
	}
}
impl SourceLocation for MatchExpressionStatement {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
pub enum MatchExpressionAntecendent {
	Expression {
		expression: Box<Expression>,
		location: SourceSpan,
	},
	Default {
		location: SourceSpan,
	},
}

impl Debug for MatchExpressionAntecendent {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::MatchExpressionAntecendent::*;
		match &self {
			Expression {
				expression,
				location: _,
			} => write!(fmt, "{:?}", expression),
			Default { location: _ } => write!(fmt, "default"),
		}
	}
}
impl SourceLocation for MatchExpressionAntecendent {
	fn get_location(&self) -> SourceSpan {
		use self::MatchExpressionAntecendent::*;
		match &self {
			Expression {
				location,
                ..
			} => *location,
			Default { location } => *location,
		}
	}
}
