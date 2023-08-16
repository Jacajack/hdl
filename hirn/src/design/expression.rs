use super::signal::{SignalSensitivity, SignalSlice, SignalClass};
use super::{Design, SignalId, DesignError};

// TODO bigint
/// Represents a numeric constant value
pub struct NumericConstant {
	// pub class: SignalClass,
	pub value: Vec<u8>,
}

impl NumericConstant {
	pub fn zero() -> NumericConstant {
		NumericConstant {
			// class: SignalClass::Unsigned(),
			value: vec![0],
		}
	}
}

/// Binary operators
/// TODO check if we have all 
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
pub struct ConditionalExpressionBranch {
	/// Condition expression
	pub condition: Expression,

	/// Value when condition is true
	pub value: Expression,
}

/// Conditional expression
/// Evaluates to the first branch where the condition is true
pub struct ConditionalExpression {
	/// Branches
	pub branches: Vec<ConditionalExpressionBranch>,

	/// Default value if all conditions are false
	pub default: Box<Expression>,
}

/// Assumption list used when evaluating an expression
pub struct Assumptions {
	pub assumptions: Vec<(SignalId, NumericConstant)>
}

/// Cast expression
pub struct CastExpression {
	/// Destination signal class
	pub dest_class: Option<SignalClass>,

	/// Destination signal sensitivity
	pub dest_sensitivity: Option<SignalSensitivity>,

	/// Source expression
	pub src: Box<Expression>,
}

/// A binary expression
pub struct BinaryExpression {
	/// Binary operator type
	pub op: BinaryOp,

	/// Left hand side expression
	pub lhs: Box<Expression>,

	/// Right hand side expression
	pub rhs: Box<Expression>,
}

/// A unary expression
pub struct UnaryExpression {
	/// Unary operator type
	pub op: UnaryOp,

	/// Operand expression
	pub operand: Box<Expression>,
}

// TODO implement Rust operator overloads
/// Language expression
pub enum Expression {
	Conditional(ConditionalExpression),
	Constant(NumericConstant),
	Signal(SignalId),
	Slice(SignalSlice),
	Binary(BinaryExpression),
	Unary(UnaryExpression),
	Cast(CastExpression),
}

impl Expression {
	/// Returns a new zero-valued expression
	pub fn new_zero() -> Self {
		Self::Constant(NumericConstant::zero())
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
		todo!();
	}

	/// Performs sign extension
	pub fn sign_extend(self, width: u32) -> Self {
		todo!();
	}

	/// Selects range of bits from the expression
	pub fn bit_select(self, msb: u32, lsb: u32) -> Self {
		assert!(msb >= lsb);
		todo!();
	}

	/// Returns sensitivity of this expression
	pub fn sensitivity(&self, design: &Design) -> SignalSensitivity {
		todo!();
	}

	/// Returns width of this expression
	pub fn width(&self, design: &Design) -> Expression {
		// TODO does this require the assumption list
		todo!();		
	}

	/// Attempts to evaluate the expression 
	pub fn eval(&self, design: &Design, assumptions: &Assumptions) -> Result<NumericConstant, DesignError> {
		todo!();
	}

	// TODO reduction AND/OR/XOR
	// TODO bitwise not
	// TODO from i32
	// TODO from u32
	// TODO from bool
	// TODO from signal ref
	// TODO from generic ref
	// TODO remaining binary ops
	

	// Casts:
	// TODO from i32
	// TODO from bool
	// TODO from Signal, SignalRef
	
}

/// Implements a conversion from signal ID to an expression
impl From<SignalId> for Expression {
	fn from(signal: SignalId) -> Self {
		Self::Signal(signal)
	}
}