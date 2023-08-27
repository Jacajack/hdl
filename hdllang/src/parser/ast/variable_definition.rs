mod pretty_printable;

use crate::lexer::CommentTableKey;
use crate::parser::ast::{DirectInitializer, SourceLocation, TypeDeclarator};
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct VariableDefinition {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub initializer_list: Vec<DirectInitializer>,
	pub location: SourceSpan,
}

impl SourceLocation for VariableDefinition {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
