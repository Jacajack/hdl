mod pretty_printable;

use crate::parser::ast::{Expression, SourceLocation};
use crate::{lexer::IdTableKey, SourceSpan};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct DirectDeclarator {
	pub name: IdTableKey,
	pub array_declarators: Vec<Expression>,
	pub location: SourceSpan,
}
impl SourceLocation for DirectDeclarator {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
