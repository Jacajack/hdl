use super::ModuleDeclarationScope;
use crate::lexer::IdTableKey;

#[derive(Debug)]
pub struct ModuleDeclared {
	pub name: IdTableKey,
	pub scope: ModuleDeclarationScope,
	pub is_generic: bool,
}
