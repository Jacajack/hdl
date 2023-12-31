use std::fmt::{Debug, Error, Formatter};

#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum RangeOpcode {
	Colon,
	PlusColon,
	ColonLessThan,
}
#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOpcode {
	BitwiseNot, // ~
	LogicalNot, // !
	Minus,      // -
	Plus,       // +
}
#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOpcode {
	Multiplication,
	Division,
	Addition,
	Subtraction,
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

impl BinaryOpcode {
	pub fn do_signs_have_to_match(&self) -> bool {
		use self::BinaryOpcode::*;
		match *self {
			Multiplication | BitwiseAnd | BitwiseOr | BitwiseXor | Addition | Subtraction => true,
			GreaterEqual | LessEqual | Equal | NotEqual | Less | Greater => true,
			_ => false,
		}
	}
	pub fn do_widths_have_to_match(&self) -> bool {
		use self::BinaryOpcode::*;
		match *self {
			BitwiseAnd | BitwiseOr | BitwiseXor => true,
			_ => false,
		}
	}
	pub fn is_logical(&self) -> bool {
		use self::BinaryOpcode::*;
		match *self {
			LogicalAnd | LogicalOr => true,
			_ => false,
		}
	}
	pub fn is_bitwise(&self) -> bool {
		use self::BinaryOpcode::*;
		match *self {
			BitwiseAnd | BitwiseOr | BitwiseXor => true,
			_ => false,
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
			Subtraction => write!(fmt, "-"),
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
impl Debug for RangeOpcode {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::RangeOpcode::*;
		match *self {
			Colon => write!(fmt, ":"),
			PlusColon => write!(fmt, "+:"),
			ColonLessThan => write!(fmt, ":<"),
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
#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq, Hash)]
pub enum AssignmentOpcode {
	Equal,
}
impl Debug for AssignmentOpcode {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		write!(fmt, "=")
	}
}
