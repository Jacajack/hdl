mod pretty_printable;

use crate::parser::ast::{Expression, SourceLocation, TypeDeclarator};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeName {
	pub declarator: TypeDeclarator,
	pub array_declarators: Vec<Expression>,
	pub location: SourceSpan,
}

impl SourceLocation for TypeName {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
