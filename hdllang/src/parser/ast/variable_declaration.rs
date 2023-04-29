use crate::parser::ast::{DirectDeclarator, SourceLocation, TypeDeclarator};
use crate::SourceSpan;
use crate::lexer::CommentTableKey;
use std::fmt::{Debug, Error, Formatter};
pub struct VariableDeclaration {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub direct_declarators: Vec<Box<DirectDeclarator>>,
	pub location: SourceSpan,
}
impl Debug for VariableDeclaration {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		write!(fmt, "{:?}", self.type_declarator)?;
		match self.direct_declarators.len() {
			0 => write!(fmt, ""),
			_ => {
				for i in 0..self.direct_declarators.len() {
					write!(fmt, " {:?}", self.direct_declarators[i])?;
				}
				write!(fmt, "")
			},
		}
	}
}
impl SourceLocation for VariableDeclaration {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
