mod pretty_printable;

use crate::lexer::CommentTableKey;
use crate::parser::ast::{SourceLocation, TypeQualifier, VariableDefinition};
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct VariableBlock {
	pub metadata: Vec<CommentTableKey>,
	pub types: Vec<TypeQualifier>,
	pub statements: Vec<VariableBlockStatement>,
	pub location: SourceSpan,
}

impl SourceLocation for VariableBlock {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum VariableBlockStatement {
	VariableDefinition(VariableDefinition),
	VariableBlock(VariableBlock),
}
impl SourceLocation for VariableBlockStatement {
	fn get_location(&self) -> SourceSpan {
		use self::VariableBlockStatement::*;
		match self {
			VariableDefinition(definition) => definition.location,
			VariableBlock(block) => block.location,
		}
	}
}
