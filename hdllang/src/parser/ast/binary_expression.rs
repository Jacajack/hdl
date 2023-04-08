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
    PostfixWithIndex {
        expression: Box<Expression>,
        index: Box<Expression>,
        location: SourceSpan,
    },
    PostfixWithRange {
        expression: Box<Expression>,
        range: Box<Expression>,
        location: SourceSpan,
    },
    PostfixWithArgs {
        expression: Box<Expression>,
        argument_list: Vec<Box<Expression>>,
        location: SourceSpan,
    },
    PostfixEmptyCall {
        expression: Box<Expression>,
        location: SourceSpan,
    },
    PostfixWithId {
        expression: Box<Expression>,
        id: IdTableKey,
        location: SourceSpan,
    },
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
pub enum TypeQualifier{
    Signed{
        location: SourceSpan,
    },
    Unsigned{
        location: SourceSpan,
    },
    Tristate{
        location: SourceSpan,
    },
    Const{
        location: SourceSpan,
    },
    Comb{
        expression: Box<Expression>,
        location: SourceSpan,
    },
    Sync{
        expression: Box<Expression>,
        location: SourceSpan,
    },
    Input{
        location: SourceSpan,
    },
    Output{
        location: SourceSpan,
    },
    Async{
        location: SourceSpan,
    },
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
                write!(fmt, "([{:?}{:?}{:?}])", lhs, code, rhs)
            }
            UnaryOperatorExpression {
                expression,
                code,
                location: _,
            } => write!(fmt, "{:?}{:?}", code, expression),
            PostfixWithId {
                expression,
                id,
                location: _,
            } => write!(fmt, "({:?}.{:?})", expression, id),
            PostfixWithIndex {
                expression,
                index,
                location: _,
            } => write!(fmt, "({:?}[{:?}])", expression, index),
            PostfixWithRange {
                expression,
                range,
                location: _,
            } => write!(fmt, "({:?}{:?})", expression, range),
            PostfixWithArgs {
                expression,
                argument_list,
                location: _,
            } => write!(fmt, "({:?}({:?}))", expression, argument_list),
            PostfixEmptyCall {
                expression,
                location: _,
            } => write!(fmt, "({:?}())", expression),
            Error => write!(fmt, "error"),
            _ => unreachable!(),
        }
    }
}

impl Debug for BinaryOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::BinaryOpcode::*;
        match *self {
            Multiplication => write!(fmt, "*"),
            Division => write!(fmt, "/"),
            Addition => write!(fmt, "+"),
            Substraction => write!(fmt, "-"),
            Modulo => write!(fmt, "%"),
            Equal => write!(fmt, "=="),
            NotEqual => write!(fmt, "!="),
            LShift => write!(fmt, "<<"),
            RShift => write!(fmt, ">>"),
            BitwiseAnd => write!(fmt, "&"),
            BitwiseOr => write!(fmt, "|"),
            BitwiseXor => write!(fmt, "^"),
            Less => write!(fmt, "<"),
            Greater => write!(fmt, ">"),
            LessEqual => write!(fmt, "<="),
            GreaterEqual => write!(fmt, ">="),
            LogicalAnd => write!(fmt, "&&"),
            LogicalOr => write!(fmt, "||"),
        }
    }
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
impl Debug for TypeQualifier {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(),Error>{
        use self::TypeQualifier::*;
        match &self{
            Signed { location:_ } => write!(fmt, "signed"),
            Unsigned { location:_ } => write!(fmt, "unsigned"),
            Tristate { location:_ } => write!(fmt, "tristate"),
            Const { location:_ } => todo!(),
            Comb { expression, location:_ } => write!(fmt, "comb({:?})",expression),
            Sync { expression, location:_ } => write!(fmt, "sync({:?})",expression),
            Input { location:_ } => write!(fmt, "input"),
            Output { location:_ } => write!(fmt, "output"),
            Async { location:_ } => write!(fmt, "async"),
        }
    }
}
impl Debug for RangeOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::RangeOpcode::*;
        match *self {
            Colon => write!(fmt, ":"),
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
            Plus => write!(fmt, "+"),
            Minus => write!(fmt, "-"),
        }
    }
}
