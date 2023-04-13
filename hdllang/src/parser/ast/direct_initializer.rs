use crate::parser::ast::{DirectDeclarator,Expression};
use crate:: SourceSpan;
use std::fmt::{Debug, Error, Formatter};
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
