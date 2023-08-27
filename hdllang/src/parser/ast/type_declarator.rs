mod pretty_printable;

use crate::parser::ast::{SourceLocation, TypeQualifier, TypeSpecifier};
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct TypeDeclarator {
	pub specifier: TypeSpecifier,
	pub qualifiers: Vec<TypeQualifier>,
	pub location: SourceSpan,
}

impl SourceLocation for TypeDeclarator {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
