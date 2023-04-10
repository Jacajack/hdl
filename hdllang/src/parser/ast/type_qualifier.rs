use crate::parser::ast::expression::Expression;
use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};
pub enum TypeQualifier {
    Signed {
        location: SourceSpan,
    },
    Unsigned {
        location: SourceSpan,
    },
    Tristate {
        location: SourceSpan,
    },
    Const {
        location: SourceSpan,
    },
    Clock {
        location: SourceSpan,
    },
    Comb {
        expression: Box<Expression>,
        location: SourceSpan,
    },
    Sync {
        expression: Box<Expression>,
        location: SourceSpan,
    },
    Input {
        location: SourceSpan,
    },
    Output {
        location: SourceSpan,
    },
    Async {
        location: SourceSpan,
    },
}
impl Debug for TypeQualifier {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::TypeQualifier::*;
        match &self {
            Signed { location: _ } => write!(fmt, "signed"),
            Unsigned { location: _ } => write!(fmt, "unsigned"),
            Tristate { location: _ } => write!(fmt, "tristate"),
            Const { location: _ } => write!(fmt, "const"),
            Comb {
                expression,
                location: _,
            } => write!(fmt, "comb({:?})", expression),
            Clock { location: _ } => write!(fmt, "clock"),
            Sync {
                expression,
                location: _,
            } => write!(fmt, "sync({:?})", expression),
            Input { location: _ } => write!(fmt, "input"),
            Output { location: _ } => write!(fmt, "output"),
            Async { location: _ } => write!(fmt, "async"),
        }
    }
}
