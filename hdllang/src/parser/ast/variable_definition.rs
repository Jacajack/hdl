use crate::parser::ast::{TypeDeclarator,DirectInitializer};
use crate:: SourceSpan;
use std::fmt::{Debug, Error, Formatter};
pub struct VariableDefinition {
    pub type_declarator: TypeDeclarator,
    pub initializer_list: Vec<DirectInitializer>,
    pub location: SourceSpan,
}
impl Debug for VariableDefinition {
    fn fmt(&self, fmt: &mut Formatter) ->  Result<(), Error>{
        write!(fmt, "{:?}", self.type_declarator)?;
        for i in 0..self.initializer_list.len() {
            write!(fmt, "{:?},", self.initializer_list[i])?;
        }
        write!(fmt, "")
    }
}
