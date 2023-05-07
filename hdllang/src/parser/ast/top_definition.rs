use crate::parser::ast::{ModuleDeclarationStatement, ModuleImplementationStatement, SourceLocation};
use crate::{lexer::CommentTableKey, lexer::IdTableKey, SourceSpan};
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Error, Formatter};

#[derive(Serialize, Deserialize)]
pub enum TopDefinition {
	ModuleDeclaration {
		metadata: Vec<CommentTableKey>,
		id: IdTableKey,
		statements: Vec<ModuleDeclarationStatement>,
		location: SourceSpan,
	},
	ModuleImplementation {
		metadata: Vec<CommentTableKey>,
		id: IdTableKey,
		statement: Box<ModuleImplementationStatement>,
		location: SourceSpan,
	},
}
impl Debug for TopDefinition {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::TopDefinition::*;
		match &self {
			ModuleDeclaration { id, statements, .. } => {
				write!(fmt, "\nmodule {:?} {{", id)?;
				for module_declaration in statements.into_iter() {
					write!(fmt, "\n{:?}", module_declaration)?;
				}
				write!(fmt, "}}")
			},
			ModuleImplementation { id, statement, .. } => {
				write!(fmt, "\nimpl {:?} {:?}", id, statement)
			},
		}
	}
}
impl SourceLocation for TopDefinition {
	fn get_location(&self) -> SourceSpan {
		use self::TopDefinition::*;
		match *self {
			ModuleImplementation { location, .. } => location,
			ModuleDeclaration { location, .. } => location,
		}
	}
}
