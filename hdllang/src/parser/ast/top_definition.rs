mod pretty_printable;

use crate::parser::ast::{ImportPath, ModuleDeclarationStatement, ModuleImplementationStatement, SourceLocation};
use crate::{lexer::CommentTableKey, lexer::IdTableKey, SourceSpan};
use serde::{Deserialize, Serialize};
use std::fmt::Debug;

#[derive(Serialize, Deserialize, Debug)]
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
	PackageDeclaration {
		metadata: Vec<CommentTableKey>,
		path: ImportPath,
		location: SourceSpan,
	},
	UseStatement {
		metadata: Vec<CommentTableKey>,
		path: ImportPath,
		location: SourceSpan,
	},
}
impl SourceLocation for TopDefinition {
	fn get_location(&self) -> SourceSpan {
		use self::TopDefinition::*;
		match *self {
			ModuleImplementation { location, .. } => location,
			ModuleDeclaration { location, .. } => location,
			PackageDeclaration { location, .. } => location,
			UseStatement { location, .. } => location,
		}
	}
}
