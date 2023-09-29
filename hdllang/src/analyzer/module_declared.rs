use hirn::design::ModuleHandle;

use crate::{lexer::IdTableKey, SourceSpan, parser::ast::DeclarationScope};

#[derive(Debug, Clone)]
pub struct ModuleDeclared {
	pub name: IdTableKey,
	pub scope: DeclarationScope,
	pub handle: ModuleHandle,
	pub is_generic: bool,
	pub location: SourceSpan,
}
