mod pretty_printable;

use crate::lexer::CommentTableKey;
use crate::parser::ast::SourceLocation;
use crate::SourceSpan;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Error, Formatter};

use super::{DirectDeclarator, TypeDeclarator, TypeQualifier};

#[derive(Serialize, Deserialize)]
pub enum ModuleDeclarationStatement {
	VariableDeclarationStatement {
		metadata: Vec<CommentTableKey>,
		type_declarator: TypeDeclarator,
		direct_declarators: Vec<DirectDeclarator>,
		location: SourceSpan,
	},
	VariableBlock {
		metadata: Vec<CommentTableKey>,
		types: Vec<TypeQualifier>,
		statements: Vec<ModuleDeclarationStatement>,
		location: SourceSpan,
	},
}
impl Debug for ModuleDeclarationStatement {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::ModuleDeclarationStatement::*;
		match &self {
			VariableDeclarationStatement {
				type_declarator,
				direct_declarators,
				..
			} => {
				write!(fmt, "{:?}", type_declarator)?;
				match direct_declarators.len() {
					0 => write!(fmt, ""),
					_ => {
						for i in 0..direct_declarators.len() {
							write!(fmt, " {:?}", direct_declarators[i])?;
						}
						write!(fmt, "")
					},
				}
			},
			VariableBlock { types, statements, .. } => match statements.len() {
				0 => writeln!(fmt, "{:?}{{}}", types),
				_ => write!(fmt, "{:?}{{\n{:?}}}\n", types, statements),
			},
		}
	}
}
impl SourceLocation for ModuleDeclarationStatement {
	fn get_location(&self) -> SourceSpan {
		use self::ModuleDeclarationStatement::*;
		match *self {
			VariableDeclarationStatement { location, .. } => location,
			VariableBlock { location, .. } => location,
		}
	}
}
