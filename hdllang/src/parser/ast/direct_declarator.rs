use crate::parser::ast::{Expression};
use crate::{lexer::IdTableKey, SourceSpan};
use std::fmt::{Debug, Error, Formatter};
pub struct DirectDeclarator {
    pub name: IdTableKey,
    pub array_declarators: Vec<Box<Expression>>,
    pub location: SourceSpan,
}
impl Debug for DirectDeclarator {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match self.array_declarators.len() {
            0 => write!(fmt, "foo"),
            _ => write!(fmt, "foo{:?}",  self.array_declarators),
        }
    }
}
