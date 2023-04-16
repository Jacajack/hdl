use crate::parser::ast::{ModuleDeclarationStatement, ModuleImplementationStatement, SourceLocation};
use crate::{lexer::IdTableKey, SourceSpan};
use std::fmt::{Debug, Error, Formatter};
pub enum TopDefinition {
	ModuleDeclaration {
		id: IdTableKey,
		statements: Vec<ModuleDeclarationStatement>,
		location: SourceSpan,
	},
	ModuleImplementation {
		id: IdTableKey,
		statement: Box<ModuleImplementationStatement>,
		location: SourceSpan,
	},
}
impl Debug for TopDefinition {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::TopDefinition::*;
		match &self {
			ModuleDeclaration {
				id: _,
				statements,
				location: _,
			} => {
				write!(fmt, "\nmodule foo {{")?;
				for module_declaration in statements.into_iter() {
					write!(fmt, "\n{:?}", module_declaration)?;
				}
				write!(fmt, "}}")
			},
			ModuleImplementation {
				id: _,
				statement,
				location: _,
			} => {
				write!(fmt, "impl foo {:?}", statement)
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
