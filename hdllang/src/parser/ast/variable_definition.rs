mod pretty_printable;

use crate::lexer::CommentTableKey;
use crate::parser::ast::{DirectInitializer, SourceLocation, TypeDeclarator};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Error, Formatter};

#[derive(Serialize, Deserialize)]
pub struct VariableDefinition {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub initializer_list: Vec<DirectInitializer>,
	pub location: SourceSpan,
}
impl Debug for VariableDefinition {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		write!(fmt, "{:?}", self.type_declarator)?;
		for i in 0..self.initializer_list.len() {
			write!(fmt, "{:?},", self.initializer_list[i])?;
		}
		write!(fmt, "")
	}
}
impl SourceLocation for VariableDefinition {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
