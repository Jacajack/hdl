use std::collections::HashMap;
use crate::lexer::IdTableKey;
use crate::SourceSpan;
use super::VariableDeclared;
#[derive(Debug)]
pub struct Scope {
	// TODO MAKE PRIVATE
	pub variables: HashMap<IdTableKey, (VariableDeclared, SourceSpan)>,
}
impl Scope {
	pub fn new() -> Self {
		Scope {
			variables: HashMap::new(),
		}
	}
	pub fn is_declared(&self, name: &IdTableKey) -> Option<SourceSpan> {
		self.variables.get(name).map(|(_, span)| *span)
	}
}