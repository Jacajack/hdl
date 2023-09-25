use super::ModuleDeclarationScope;
use crate::{lexer::IdTableKey, SourceSpan};

#[derive(Debug, Clone)]
pub struct ModuleDeclared {
	pub name: IdTableKey,
	pub scope: ModuleDeclarationScope,
	pub is_generic: bool,
	pub location: SourceSpan,
}
