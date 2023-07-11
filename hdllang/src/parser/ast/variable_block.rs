use crate::lexer::CommentTableKey;
use crate::parser::ast::{SourceLocation, TypeQualifier, VariableDeclaration};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Error, Formatter};

#[derive(Serialize, Deserialize)]
pub struct VariableBlock {
	pub metadata: Vec<CommentTableKey>,
	pub types: Vec<TypeQualifier>,
	pub statements: Vec<VariableBlockStatement>,
	pub location: SourceSpan,
}
impl Debug for VariableBlock {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		match self.statements.len() {
			0 => writeln!(fmt, "{:?}{{}}", self.types),
			_ => write!(fmt, "{:?}{{\n{:?}}}\n", self.types, self.statements),
		}
	}
}
impl SourceLocation for VariableBlock {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}

#[derive(Serialize, Deserialize)]
pub enum VariableBlockStatement {
	VariableDeclarationStatement {
		declaration: VariableDeclaration,
		location: SourceSpan,
	},
	VariableBlock {
		block: Box<VariableBlock>,
		location: SourceSpan,
	},
}
impl Debug for VariableBlockStatement {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::VariableBlockStatement::*;
		match &self {
			VariableDeclarationStatement {
				declaration,
				location: _,
			} => write!(fmt, "{:?};", declaration),
			VariableBlock { block, location: _ } => write!(fmt, "{:?}", block),
		}
	}
}
impl SourceLocation for VariableBlockStatement {
	fn get_location(&self) -> SourceSpan {
		use self::VariableBlockStatement::*;
		match *self {
			VariableDeclarationStatement { location, .. } => location,
			VariableBlock { location, .. } => location,
		}
	}
}
