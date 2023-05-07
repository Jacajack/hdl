use crate::parser::ast::{SourceLocation, TypeQualifier, TypeSpecifier};
use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub struct TypeDeclarator {
	pub specifier: TypeSpecifier,
	pub qualifiers: Vec<TypeQualifier>,
	pub location: SourceSpan,
}
impl Debug for TypeDeclarator {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		match self.qualifiers.len() {
			0 => write!(fmt, "{:?}", self.specifier),
			_ => {
				for i in 0..self.qualifiers.len() {
					write!(fmt, "{:?} ", self.qualifiers[i])?;
				}
				write!(fmt, "{:?}", self.specifier)
			},
		}
	}
}
impl SourceLocation for TypeDeclarator {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
