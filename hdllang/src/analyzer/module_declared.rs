use crate::lexer::IdTableKey;
use super::Scope;

#[derive(Debug)]
pub struct ModuleDeclared {
	pub name: IdTableKey,
	pub scope: Scope,
	pub is_generic: bool,
}