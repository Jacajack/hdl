mod pretty_printable;

use crate::parser::ast::{Expression, SourceLocation, TypeDeclarator};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Error, Formatter};

#[derive(Serialize, Deserialize)]
pub struct TypeName {
	pub declarator: TypeDeclarator,
	pub array_declarators: Vec<Box<Expression>>,
	pub location: SourceSpan,
}
impl Debug for TypeName {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		match self.array_declarators.len() {
			0 => write!(fmt, "{:?}", self.declarator),
			_ => write!(fmt, "{:?}{:?}", self.declarator, self.array_declarators),
		}
	}
}
impl SourceLocation for TypeName {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
