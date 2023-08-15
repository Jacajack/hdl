mod pretty_printable;

use crate::lexer::CommentTableKey;
use crate::parser::ast::{DirectInitializer, SourceLocation, TypeDeclarator};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize,Debug)]
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
