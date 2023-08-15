mod pretty_printable;

use crate::SourceSpan;

use crate::parser::ast::{Expression, SourceLocation};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct MatchExpressionStatement {
	pub antecedent: MatchExpressionAntecendent,
	pub expression: Expression,
	pub location: SourceSpan,
}

impl SourceLocation for MatchExpressionStatement {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum MatchExpressionAntecendent {
	Expression {
		expressions: Vec<Expression>,
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
