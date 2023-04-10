use std::fmt::{Debug, Error, Formatter};

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

impl Debug for BinaryOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::BinaryOpcode::*;
        match *self {
            Multiplication => write!(fmt, "*"),
            Division       => write!(fmt, "/"),
            Addition       => write!(fmt, "+"),
            Substraction   => write!(fmt, "-"),
            Modulo         => write!(fmt, "%"),
            Equal          => write!(fmt, "=="),
            NotEqual       => write!(fmt, "!="),
            LShift         => write!(fmt, "<<"),
            RShift         => write!(fmt, ">>"),
            BitwiseAnd     => write!(fmt, "&"),
            BitwiseOr      => write!(fmt, "|"),
            BitwiseXor     => write!(fmt, "^"),
            Less           => write!(fmt, "<"),
            Greater        => write!(fmt, ">"),
            LessEqual      => write!(fmt, "<="),
            GreaterEqual   => write!(fmt, ">="),
            LogicalAnd     => write!(fmt, "&&"),
            LogicalOr      => write!(fmt, "||"),
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
#[derive(Copy, Clone)]
pub enum AssignmentOpcode {
    Equal,
    PlusEqual,
    AndEqual,
    XorEqual,
    OrEqual,
}
impl Debug for AssignmentOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::AssignmentOpcode::*;
        match *self {
            Equal     => write!(fmt, "="),
            PlusEqual => write!(fmt, "+="),
            AndEqual  => write!(fmt, "&="),
            XorEqual  => write!(fmt, "^="),
            OrEqual   => write!(fmt, "|="),
        }
    }
}