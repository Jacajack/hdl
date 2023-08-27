mod pretty_printable;

use crate::parser::ast::expression::Expression;
use crate::parser::ast::SourceLocation;
use crate::SourceSpan;
use num_bigint::BigInt;
use std::fmt::{Debug, Error, Formatter};
#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq)]
pub struct Bus {
	pub width: Box<Expression>,
	pub location: SourceSpan,
	pub compiled_width: Option<BigInt> // to be transferred to a hashmap,
}
#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq)]
pub enum TypeSpecifier {
	Auto {
		location: SourceSpan,
	},
	Int {
		location: SourceSpan,
	},
	Wire {
		location: SourceSpan,
	},
	Bool {
		location: SourceSpan,
	},
	Bus (Bus),
}
impl Debug for TypeSpecifier {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::TypeSpecifier::*;
		match &self {
			Auto { location: _ } => write!(fmt, "auto"),
			Int { location: _ } => write!(fmt, "int"),
			Bool { location: _ } => write!(fmt, "bool"),
			Wire { location: _ } => write!(fmt, "wire"),
			Bus (bus) => write!(fmt, "bus<{:?}>", bus.width),
		}
	}
}
impl SourceLocation for TypeSpecifier {
	fn get_location(&self) -> SourceSpan {
		use self::TypeSpecifier::*;
		match self {
			Auto { location } => *location,
			Int { location } => *location,
			Wire { location } => *location,
			Bool { location } => *location,
			Bus (bus) => bus.location,
		}
	}
}
