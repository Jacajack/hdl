use crate::parser::ast::{Expression, SourceLocation};
use crate::{lexer::IdTableKey, SourceSpan};
use std::fmt::{Debug, Error, Formatter};
pub struct DirectDeclarator {
	pub name: IdTableKey,
	pub array_declarators: Vec<Box<Expression>>,
	pub location: SourceSpan,
}
impl Debug for DirectDeclarator {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		write!(fmt, "{:?}", self.name)?;
		for array in &self.array_declarators{
			write!(fmt, "[{:?}]", array)?;
		}
		Ok(())
	}
}
impl SourceLocation for DirectDeclarator {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
