mod pretty_printable;

use crate::parser::ast::{Expression, SourceLocation};
use crate::{lexer::IdTableKey, SourceSpan};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
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
