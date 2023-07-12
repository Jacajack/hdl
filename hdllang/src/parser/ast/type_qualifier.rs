mod pretty_printable;

use crate::parser::ast::{Expression, SourceLocation};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;

#[derive(Serialize, Deserialize, Debug)]
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
	Comb {
		expression: Expression,
		location: SourceSpan,
	},
	Sync {
		expressions: Vec<Expression>,
		location: SourceSpan,
	},
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
		match *self {
			Signed { location } => location,
			Unsigned { location } => location,
			Tristate { location } => location,
			Const { location } => location,
			Clock { location } => location,
			Comb { location, .. } => location,
			Sync { location, .. } => location,
			Input { location } => location,
			Output { location } => location,
			Async { location } => location,
		}
	}
}
