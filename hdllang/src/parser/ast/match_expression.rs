mod pretty_printable;

use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};

use crate::parser::ast::{Expression, SourceLocation};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
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
#[derive(Serialize, Deserialize, Debug)]
pub enum MatchExpressionAntecendent {
	Expression {
		expressions: Vec<Box<Expression>>,
		location: SourceSpan,
	},
	Default {
		location: SourceSpan,
	},
}

impl SourceLocation for MatchExpressionAntecendent {
	fn get_location(&self) -> SourceSpan {
		use self::MatchExpressionAntecendent::*;
		match *self {
			Expression { location, .. } => location,
			Default { location } => location,
		}
	}
}
