use std::fmt::{Debug, Error, Formatter};
use crate::SourceSpan;

use crate::parser::ast::expression::Expression;

pub struct MatchExpressionStatement {
    pub antecedent: MatchExpressionAntecendent,
    pub expression: Box<Expression>,
    pub location: SourceSpan,
}

impl Debug for MatchExpressionStatement {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{:?} => {:?}", self.antecedent, self.expression)
    }
}

pub enum MatchExpressionAntecendent {
    Expression {
        expression: Box<Expression>,
        location: SourceSpan,
    },
    Default {
        location: SourceSpan,
    },
}

impl Debug for MatchExpressionAntecendent {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::MatchExpressionAntecendent::*;
        match &self {
            Expression {
                expression,
                location: _,
            } => write!(fmt, "{:?}", expression),
            Default { location: _ } => write!(fmt, "default"),
        }
    }
}
