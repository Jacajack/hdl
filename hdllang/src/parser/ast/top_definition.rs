mod pretty_printable;
mod module_declaration;
mod package_declaration;
mod module_implementation;

pub use module_implementation::ModuleImplementation;
pub use module_declaration::ModuleDeclaration;
pub use package_declaration::PackageDeclaration;

use crate::parser::ast::{ImportPath, SourceLocation};
use crate::{lexer::CommentTableKey, SourceSpan};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum TopDefinition {
	ModuleDeclaration(ModuleDeclaration),
	ModuleImplementation(ModuleImplementation),
	PackageDeclaration(PackageDeclaration),
	UseStatement(UseStatement),
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct UseStatement {
	pub metadata: Vec<CommentTableKey>,
	pub path: ImportPath,
	pub location: SourceSpan,
}

impl SourceLocation for TopDefinition {
	fn get_location(&self) -> SourceSpan {
		use self::TopDefinition::*;
		match self {
			ModuleImplementation(implementation) => implementation.location,
			PackageDeclaration(package) => package.location,
			UseStatement(use_statement) => use_statement.location,
			ModuleDeclaration(declaration) => declaration.location,
		}
	}
}
