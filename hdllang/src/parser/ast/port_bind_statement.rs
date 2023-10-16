mod pretty_printable;

use crate::parser::ast::{Expression, SourceLocation, VariableDeclaration};
use crate::{lexer::IdTableKey, SourceSpan};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdWithDeclaration {
	pub id: IdTableKey,
	pub declaration: VariableDeclaration,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdWithExpression {
	pub id: IdTableKey,
	pub expression: Expression,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct OnlyId {
	pub id: IdTableKey,
	pub location: SourceSpan,
}
impl SourceLocation for PortBindStatement {
	fn get_location(&self) -> SourceSpan {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.location,
			IdWithExpression(id_with_expression) => id_with_expression.location,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.location,
		}
	}
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum PortBindStatement {
	OnlyId(OnlyId),
	IdWithExpression(IdWithExpression),
	IdWithDeclaration(IdWithDeclaration),
}

impl PortBindStatement {
	pub fn get_id(&self) -> IdTableKey {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.id,
			IdWithExpression(id_with_expression) => id_with_expression.id,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.id,
		}
	}
	pub fn location(&self) -> SourceSpan {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.location,
			IdWithExpression(id_with_expression) => id_with_expression.location,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.location,
		}
	}
}
