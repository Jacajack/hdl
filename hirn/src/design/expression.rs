use super::signal::{SignalSensitivity, SignalSlice, SignalClass};
use super::{SignalId, NumericConstant};

/// Binary operators
/// TODO check if we have all 
#[derive(Clone, Copy)]
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

/// Unary operators
/// TODO check if we have all
#[derive(Clone, Copy)]
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

/// Represents a conditional expression branch
#[derive(Clone)]
pub struct ConditionalExpressionBranch {
	/// Condition expression
	pub condition: Expression,

	/// Value when condition is true
	pub value: Expression,
}

/// Conditional expression
/// Evaluates to the first branch where the condition is true
#[derive(Clone)]
pub struct ConditionalExpression {
	/// Branches
	pub branches: Vec<ConditionalExpressionBranch>,

	/// Default value if all conditions are false
	pub default: Box<Expression>,
}

/// Cast expression
#[derive(Clone)]
pub struct CastExpression {
	/// Destination signal class
	pub dest_class: Option<SignalClass>,

	/// Destination signal sensitivity
	pub dest_sensitivity: Option<SignalSensitivity>,

	/// Source expression
	pub src: Box<Expression>,
}

/// A binary expression
#[derive(Clone)]
pub struct BinaryExpression {
	/// Binary operator type
	pub op: BinaryOp,

	/// Left hand side expression
	pub lhs: Box<Expression>,

	/// Right hand side expression
	pub rhs: Box<Expression>,
}

/// A unary expression
#[derive(Clone)]
pub struct UnaryExpression {
	/// Unary operator type
	pub op: UnaryOp,

	/// Operand expression
	pub operand: Box<Expression>,
}

// TODO implement Rust operator overloads
/// Language expression
#[derive(Clone)]
pub enum Expression {
	Conditional(ConditionalExpression),
	Constant(NumericConstant),
	Signal(SignalId),
	Slice(SignalSlice),
	Binary(BinaryExpression),
	Unary(UnaryExpression),
	Cast(CastExpression),
	// TODO bit-select expression
}

impl Expression {
	/// Returns a new zero-valued expression
	pub fn new_zero() -> Self {
		Self::Constant(NumericConstant::zero())
	}

	/// Returns a new one-valued expression
	pub fn new_one() -> Self {
		Self::Constant(NumericConstant::one())
	}

	/// Cassts expression to a different type
	pub fn cast(self, dest_class: Option<SignalClass>, dest_sensitivity: Option<SignalSensitivity>) -> Self {
		Self::Cast(CastExpression {
			dest_class,
			dest_sensitivity,
			src: Box::new(self)
		})
	}

	/// Performs zero extension
	pub fn zero_extend(self, width: u32) -> Self {
		Self::Unary(UnaryExpression {
			op: UnaryOp::ZeroExtend{width},
			operand: Box::new(self),
		})
	}

	/// Performs sign extension
	pub fn sign_extend(self, width: u32) -> Self {
		Self::Unary(UnaryExpression {
			op: UnaryOp::SignExtend{width},
			operand: Box::new(self),
		})
	}

	/// Selects range of bits from the expression
	pub fn bit_select(self, msb: u32, lsb: u32) -> Self {
		assert!(msb >= lsb);
		todo!();
	}

	/// Attempt to drive the expression if possible.
	/// Returns affected signal slice if drivable.
	pub fn try_drive(&self) -> Option<SignalSlice> {
		match self {
			Self::Signal(ref sig)  => Some((*sig).into()),
			Self::Slice(slice) => Some(slice.clone()),
			_ => None,
		}
	}

	// TODO reduction AND/OR/XOR
	// TODO bitwise not
	// TODO from i32
	// TODO from u32
	// TODO from bool
	// TODO remaining binary ops
	

	// Casts:
	// TODO from bool
	
}

/// Implements a conversion from signal ID to an expression
impl From<SignalId> for Expression {
	fn from(signal: SignalId) -> Self {
		Self::Signal(signal)
	}
}

impl From<NumericConstant> for Expression {
	fn from(constant: NumericConstant) -> Self {
		Self::Constant(constant)
	}
}

impl From<u64> for Expression {
	fn from(value: u64) -> Self {
		Self::Constant(value.into())
	}
}

impl From<i64> for Expression {
	fn from(value: i64) -> Self {
		Self::Constant(value.into())
	}
}