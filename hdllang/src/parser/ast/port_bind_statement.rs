use crate::parser::ast::{Expression, SourceLocation, VariableDeclaration};
use crate::{lexer::IdTableKey, SourceSpan};
use std::fmt::{Debug, Error, Formatter};
pub enum PortBindStatement {
	OnlyId {
		id: IdTableKey,
		location: SourceSpan,
	},
	IdWithExpression {
		id: IdTableKey,
		expression: Box<Expression>,
		location: SourceSpan,
	},
	IdWithDeclaration {
		id: IdTableKey,
		declaration: VariableDeclaration,
		location: SourceSpan,
	},
}
impl Debug for PortBindStatement {
	fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), Error> {
		use self::PortBindStatement::*;
		match &self {
			OnlyId { id: _, location: _ } => write!(fmt, "foo"),
			IdWithExpression {
				id: _,
				expression,
				location: _,
			} => write!(fmt, "foo: {:?}", expression),
			IdWithDeclaration {
				id: _,
				declaration,
				location: _,
			} => write!(fmt, "foo: {:?}", declaration),
		}
	}
}
impl SourceLocation for PortBindStatement {
	fn get_location(&self) -> SourceSpan {
		use self::PortBindStatement::*;
		match *self {
			OnlyId { location, .. } => location,
			IdWithExpression { location, .. } => location,
			IdWithDeclaration { location, .. } => location,
		}
	}
}
