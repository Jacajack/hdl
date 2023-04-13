use crate::parser::ast::expression::Expression;
use crate:: SourceSpan;
use crate::parser::ast::SourceLocation;
use std::fmt::{Debug, Error, Formatter};

pub enum TypeSpecifier {
    Auto {
        location: SourceSpan,
    },
    Int {
        location: SourceSpan,
    },
    Wire {
        location: SourceSpan,
    },
    Bool {
        location: SourceSpan,
    },
    Bus {
        width: Box<Expression>,
        location: SourceSpan,
    },
}
impl Debug for TypeSpecifier {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::TypeSpecifier::*;
        match &self {
            Auto { location: _ } => write!(fmt, "auto"),
            Int { location: _ } => write!(fmt, "int"),
            Bool { location: _ } => write!(fmt, "bool"),
            Wire { location: _ } => write!(fmt, "wire"),
            Bus { width, location: _ } => write!(fmt, "bus<{:?}>", width),
        }
    }
}
impl SourceLocation for TypeSpecifier{
    fn get_location(&self)->SourceSpan {
        use self::TypeSpecifier::*;
        match &self {
            Auto { location } => *location,
            Int { location } => *location,
            Wire { location } => *location,
            Bool { location } => *location,
            Bus { location,.. } => *location,
        }
    }
}