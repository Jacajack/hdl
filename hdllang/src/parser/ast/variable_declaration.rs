mod pretty_printable;

use crate::lexer::CommentTableKey;
use crate::parser::ast::{DirectDeclarator, SourceLocation, TypeDeclarator};
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct VariableDeclaration {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub direct_declarators: Vec<DirectDeclarator>,
	pub location: SourceSpan,
}

impl SourceLocation for VariableDeclaration {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
