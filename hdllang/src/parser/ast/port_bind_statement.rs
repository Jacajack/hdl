mod pretty_printable;

use crate::parser::ast::{Expression, SourceLocation, VariableDeclaration};
use crate::{lexer::IdTableKey, SourceSpan};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct IdWithDeclaration {
	pub id: IdTableKey,
	pub declaration: VariableDeclaration,
	pub location: SourceSpan,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct IdWithExpression {
	pub id: IdTableKey,
	pub expression: Expression,
	pub location: SourceSpan,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct OnlyId {
	pub id: IdTableKey,
	pub location: SourceSpan,
}
impl SourceLocation for PortBindStatement {
	fn get_location(&self) -> SourceSpan {
		use self::PortBindStatement::*;
		match self {
			OnlyId (only_id) => only_id.location,
			IdWithExpression (id_with_expression) => id_with_expression.location,
			IdWithDeclaration (id_with_declaration) => id_with_declaration.location,
		}
	}
}

#[derive(Serialize, Deserialize, Debug)]
pub enum PortBindStatement {
	OnlyId (OnlyId),
	IdWithExpression (IdWithExpression),
	IdWithDeclaration (IdWithDeclaration),
}

