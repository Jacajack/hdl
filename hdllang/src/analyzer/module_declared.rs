use crate::lexer::IdTableKey;
use super::ModuleDeclarationScope;

#[derive(Debug)]
pub struct ModuleDeclared {
	pub name: IdTableKey,
	pub scope: ModuleDeclarationScope,
	pub is_generic: bool,
}