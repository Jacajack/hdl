use num_bigint::BigInt;

use crate::{lexer::IdTableKey, parser::ast::{TypeSpecifier, Expression}};

use super::CombinedQualifiers;

#[derive(Debug, Clone)]
pub struct VariableDeclared {
	pub name: IdTableKey,
	pub qualifiers: CombinedQualifiers,
	pub specifier: TypeSpecifier,
	pub array: Vec<Expression>,
	pub array_compiled: Option<Vec<BigInt>>,
}