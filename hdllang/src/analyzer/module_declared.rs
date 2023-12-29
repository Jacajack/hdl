use hirn::design::ModuleHandle;

use crate::{lexer::IdTableKey, SourceSpan};

use super::LocalAnalyzerContext;

#[derive(Clone)]
pub struct ModuleDeclared {
	pub name: IdTableKey,
	pub context: Box<LocalAnalyzerContext>,
	pub handle: ModuleHandle,
	pub is_generic: bool,
	pub location: SourceSpan,
	pub instantiates: Vec<IdTableKey>, // FIXME probably import path or unique id
}
