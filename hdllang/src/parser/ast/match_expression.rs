mod pretty_printable;

use crate::SourceSpan;

use crate::parser::ast::{Expression, SourceLocation};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
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
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
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
