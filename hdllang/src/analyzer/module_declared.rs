use hirn::design::ModuleHandle;

use crate::{lexer::IdTableKey, SourceSpan};

use super::ModuleImplementationScope;

#[derive(Debug, Clone)]
pub struct ModuleDeclared {
	pub name: IdTableKey,
	pub scope: ModuleImplementationScope,
	pub handle: ModuleHandle,
	pub is_generic: bool,
	pub location: SourceSpan,
	pub instatiaed: Vec<IdTableKey> // FIXME probably import path or unique id
}
