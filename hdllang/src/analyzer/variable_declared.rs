use crate::{lexer::IdTableKey, parser::ast::{TypeSpecifier, Expression}};

use super::CombinedQualifiers;

#[derive(Debug)]
pub struct VariableDeclared {
	pub name: IdTableKey,
	pub qualifiers: CombinedQualifiers,
	pub specifier: TypeSpecifier,
	pub array: Vec<Expression>,
}