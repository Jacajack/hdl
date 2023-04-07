use crate::{lexer::IdTableKey, SourceSpan};
use std::fmt::{Debug, Error, Formatter};

pub enum Expression {
    Number {
        value: u64,
        location: SourceSpan,
    },
    Identifier {
        id: IdTableKey,
        location: SourceSpan,
    },
    ParenthesizedExpression {
        expression: Box<Expression>,
        location: SourceSpan,
    },
    MatchExpression,
    ConditionalExpression,
    Tuple,
    TernaryExpression {
        condition: Box<Expression>,
        true_branch: Box<Expression>,
        false_branch: Box<Expression>,
        location: SourceSpan,
    },
    PostfixExpression(Box<PostfixExpression>),
    TypeNameCastExpression,
    UnaryOperatorExpression {
        expression: Box<Expression>,
        code: UnaryOpcode,
        location: SourceSpan,
    },
    RangeExpression {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        code: RangeOpcode,
        location: SourceSpan,
    },
    BinaryExpression {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        code: BinaryOpcode,
        location: SourceSpan,
    },
    Error,
}

pub enum PostfixExpression {
    PostfixWithIndex,
    PostfixWithRange,
    PostfixWithArgs,
    PostfixWithId(Box<Expression>, usize, usize, IdTableKey),
}
#[derive(Copy, Clone)]
pub enum RangeOpcode {
    Colon,
    PlusColon,
}
#[derive(Copy, Clone)]
pub enum UnaryOpcode {
    BitwiseNot,
    LogicalNot,
    Minus,
    Plus,
}
#[derive(Copy, Clone)]
pub enum BinaryOpcode {
    Multiplication,
    Division,
    Addition,
    Substraction,
    Modulo,
    Equal,
    NotEqual,
    LShift,
    RShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
}

impl Debug for Expression {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Expression::*;
        match &self {
            Number { value, location: _ } => write!(fmt, "{:?}", value),
            Identifier { id: _, location: _ } => write!(fmt, "foo"),
            ParenthesizedExpression {
                expression,
                location: _,
            } => write!(fmt, "({:?})", *expression),
            BinaryExpression {
                lhs,
                rhs,
                code,
                location: _,
            } => {
                write!(fmt, "({:?} {:?} {:?})", lhs, code, rhs)
            }
            TernaryExpression {
                condition,
                true_branch,
                false_branch,
                location: _,
            } => {
                write!(
                    fmt,
                    "({:?} ? {:?} : {:?})",
                    condition, true_branch, false_branch
                )
            }
            RangeExpression {
                lhs,
                rhs,
                location: _,
                code,
            } => {
                write!(fmt, "({:?} {:?} {:?})", lhs, code, rhs)
            }
            UnaryOperatorExpression {
                expression,
                code,
                location: _,
            } => write!(fmt, "{:?}{:?}", code, expression),
            Error => write!(fmt, "error"),
            _ => write!(fmt, "dupa"),
        }
    }
}

impl Debug for BinaryOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::BinaryOpcode::*;
        match *self {
            Multiplication  => write!(fmt, "*"),
            Division        => write!(fmt, "/"),
            Addition        => write!(fmt, "+"),
            Substraction    => write!(fmt, "-"),
            Modulo          => write!(fmt, "%"),
            Equal           => write!(fmt, "=="),
            NotEqual        => write!(fmt, "!="),
            LShift          => write!(fmt, "<<"),
            RShift          => write!(fmt, ">>"),
            BitwiseAnd      => write!(fmt, "&"),
            BitwiseOr       => write!(fmt, "|"),
            BitwiseXor      => write!(fmt, "^"),
            Less            => write!(fmt, "<"),
            Greater         => write!(fmt, ">"),
            LessEqual       => write!(fmt, "<="),
            GreaterEqual    => write!(fmt, ">="),
            LogicalAnd      => write!(fmt, "&&"),
            LogicalOr       => write!(fmt, "||"),
        }
    }
}
impl Debug for RangeOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::RangeOpcode::*;
        match *self {
            Colon     => write!(fmt, ":"),
            PlusColon => write!(fmt, "+:"),
        }
    }
}
impl Debug for UnaryOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::UnaryOpcode::*;
        match *self {
            BitwiseNot => write!(fmt, "~"),
            LogicalNot => write!(fmt, "!"),
            Plus       => write!(fmt, "+"),
            Minus      => write!(fmt, "-"),
        }
    }
}
