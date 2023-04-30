use crate::parser::ast::{DirectDeclarator, Expression, SourceLocation};
use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub enum DirectInitializer {
	DirectDeclarator {
		declarator: Box<DirectDeclarator>,
		location: SourceSpan,
	},
	DirectDeclaratorWithInitializer {
		declarator: Box<DirectDeclarator>,
		expression: Box<Expression>,
		location: SourceSpan,
	},
}
impl Debug for DirectInitializer {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::DirectInitializer::*;
		match &self {
			DirectDeclarator {
				declarator,
				location: _,
			} => write!(fmt, "{:?}", declarator),
			DirectDeclaratorWithInitializer {
				declarator,
				expression,
				location: _,
			} => write!(fmt, "{:?} = {:?}", declarator, expression),
		}
	}
}
impl SourceLocation for DirectInitializer {
	fn get_location(&self) -> SourceSpan {
		use self::DirectInitializer::*;
		match &self {
			DirectDeclarator { location, .. } => *location,
			DirectDeclaratorWithInitializer { location, .. } => *location,
		}
	}
}
