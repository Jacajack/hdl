use crate::parser::ast::{DirectDeclarator, Expression, SourceLocation};
use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub struct DirectInitializer {
	pub declarator: Box<DirectDeclarator>,
	pub expression: Option<Box<Expression>>,
	pub location: SourceSpan,	
}
impl Debug for DirectInitializer {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		write!(fmt, "{:?}", self.declarator)?;
		if let Some(expr) = &self.expression {
			write!(fmt, " = {:?}", expr)?;
		}
		write!(fmt, "")
	}
}
impl SourceLocation for DirectInitializer {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
