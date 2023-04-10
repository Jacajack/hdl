use crate:: SourceSpan;
use crate::parser::ast::{TypeDeclarator,Expression};
use std::fmt::{Debug, Error, Formatter};
pub struct TypeName {
    pub declarator: TypeDeclarator,
    pub array_declarators: Vec<Box<Expression>>,
    pub location: SourceSpan,
}
impl Debug for TypeName {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match self.array_declarators.len() {
            0 => write!(fmt, "{:?}", self.declarator),
            _ => write!(fmt, "{:?}{:?}", self.declarator, self.array_declarators),
        }
    }
}