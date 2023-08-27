mod pretty_printable;

use crate::parser::ast::{Expression, SourceLocation};
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct 	Sync {
	pub expressions: Vec<Expression>,
	pub location: SourceSpan,
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct Comb {
	pub expressions: Vec<Expression>,
	pub location: SourceSpan,
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum TypeQualifier {
	Signed {
		location: SourceSpan,
	},
	Unsigned {
		location: SourceSpan,
	},
	Tristate {
		location: SourceSpan,
	},
	Const {
		location: SourceSpan,
	},
	Clock {
		location: SourceSpan,
	},
	Comb (Comb),
	Sync (Sync),
	Input {
		location: SourceSpan,
	},
	Output {
		location: SourceSpan,
	},
	Async {
		location: SourceSpan,
	},
}
impl SourceLocation for TypeQualifier {
	fn get_location(&self) -> SourceSpan {
		use TypeQualifier::*;
		match self {
			Signed { location } => *location,
			Unsigned { location } => *location,
			Tristate { location } => *location,
			Const { location } => *location,
			Clock { location } => *location,
			Comb (comb) => comb.location,
			Sync (sync) => sync.location,
			Input { location } => *location,
			Output { location } => *location,
			Async { location } => *location,
		}
	}
}
