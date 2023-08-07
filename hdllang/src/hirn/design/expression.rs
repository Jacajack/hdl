use super::signal::{SignalRef, SignalClass, SignalType};

pub struct NumericConstant {
	pub width: u32,
	pub class: SignalClass,
	pub value: Vec<u8>,
}

impl NumericConstant {
	pub fn zero() -> NumericConstant {
		NumericConstant {
			width: 1,
			class: SignalClass::Unsigned,
			value: vec![0],
		}
	}
}

// TODO check if we have all 
pub enum BinaryOp {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	ShiftLeft,
	ShiftRight,
	LogicalAnd,
	LogicalOr,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	Join,
	Equal,
	NotEqual,
	Less,
	LessEqual,
	Greater,
	GreaterEqual,
}

// TODO check if we have all
pub enum UnaryOp {
	Negate,
	LogicalNot,
	BitwiseNot,
	ZeroExtend{width: u32},
	SignExtend{width: u32},
	BitSelect(u32, u32),
	ReductionAnd,
	ReductionOr,
	ReductionXor,
}

pub struct ConditionalExpressionBranch {
	pub condition: Expression,
	pub value: Expression,
}

pub struct ConditionalExpression {
	pub branches: Vec<ConditionalExpressionBranch>,
	pub default: Box<Expression>,
}

// TODO implement Rust operator overloads
pub enum Expression {
	Conditional(ConditionalExpression),
	Constant(NumericConstant),
	Signal(SignalRef),
	Binary{op: BinaryOp, lhs: Box<Expression>, rhs: Box<Expression>},
	Unary{op: UnaryOp, operand: Box<Expression>},
	Cast{dest_type: SignalType, src: Box<Expression>},
}

impl Expression {

	pub fn cast(self, dest_type: SignalType) -> Self {
		Self::Cast{dest_type, src: Box::new(self)}
	}

	pub fn zero_extend(self, width: u32) -> Self {
		Self::Unary{op: UnaryOp::ZeroExtend{width}, operand: Box::new(self)}
	}

	pub fn sign_extend(self, width: u32) -> Self {
		Self::Unary{op: UnaryOp::SignExtend{width}, operand: Box::new(self)}
	}

	pub fn bit_select(self, msb: u32, lsb: u32) -> Self {
		assert!(msb >= lsb);
		Self::Unary{op: UnaryOp::BitSelect(msb, lsb), operand: Box::new(self)}
	}

	// TODO reduction AND/OR/XOR
	// TODO bitwise not
	// TODO from i32
	// TODO from u32
	// TODO from bool
	// TODO from signal ref
	// TODO from generic ref
	// TODO remaining binary ops
}

pub trait IsCompileTimeConst {
	fn is_compile_time_const(&self) -> bool;
}

pub trait HasBitWidth {
	fn get_bit_width(&self) -> u32;
}

impl IsCompileTimeConst for ConditionalExpression {
	fn is_compile_time_const(&self) -> bool {
		self.default.is_compile_time_const() && self.branches.iter().all(|branch| {
			branch.condition.is_compile_time_const() && branch.value.is_compile_time_const()
		})
	}
}

impl IsCompileTimeConst for Expression {
	fn is_compile_time_const(&self) -> bool {
		use Expression::*;
		match self {
			Conditional(cond) => cond.is_compile_time_const(),
			Constant{..} => true,
			_ => false,
		}
	}
}
