use super::signal::{SignalClass, SignalSensitivity, SignalSlice};
use super::{NumericConstant, SignalId};

/// Binary operators
/// TODO check if we have all
#[derive(Clone, Copy, Debug)]
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
	Equal,
	NotEqual,
	Less,
	LessEqual,
	Greater,
	GreaterEqual,
}

/// Unary operators
/// TODO check if we have all
#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
	Negate,
	LogicalNot,
	BitwiseNot,
	ReductionAnd,
	ReductionOr,
	ReductionXor,
}

#[derive(Clone, Debug)]
pub enum BuiltinOp {
	ZeroExtend {
		expr: Box<Expression>,
		width: Box<Expression>,
	},
	SignExtend {
		expr: Box<Expression>,
		width: Box<Expression>,
	},
	BitSelect {
		expr: Box<Expression>,
		msb: Box<Expression>,
		lsb: Box<Expression>,
	},
	BusSelect {
		expr: Box<Expression>,
		index: Box<Expression>,
	},
	Replicate {
		expr: Box<Expression>,
		count: Box<Expression>,
	},
	Join(Vec<Expression>),
}

/// Represents a conditional expression branch
#[derive(Clone, Debug)]
pub struct ConditionalExpressionBranch {
	/// Condition expression
	pub condition: Expression,

	/// Value when condition is true
	pub value: Expression,
}

/// Conditional expression
/// Evaluates to the first branch where the condition is true
#[derive(Clone, Debug)]
pub struct ConditionalExpression {
	/// Branches
	branches: Vec<ConditionalExpressionBranch>,

	/// Default value if all conditions are false
	default: Box<Expression>,
}

impl ConditionalExpression {
	fn new(default: Expression) -> Self {
		Self {
			branches: Vec::new(),
			default: Box::new(default),
		}
	}

	fn add_branch(&mut self, condition: Expression, value: Expression) {
		self.branches.push(ConditionalExpressionBranch {
			condition,
			value,
		});
	}
}

/// A helper class for constructing conditional/match expressions
#[derive(Clone, Debug)]
pub struct ConditionalExpressionBuilder {
	expr: ConditionalExpression
}

impl ConditionalExpressionBuilder {
	pub fn new(default: Expression) -> Self {
		Self {
			expr: ConditionalExpression::new(default)
		}
	}

	pub fn branch(mut self, condition: Expression, value: Expression) -> Self {
		self.expr.add_branch(condition, value);
		self
	}

	pub fn build(self) -> Expression {
		self.expr.into()
	}
}

/// Cast expression
#[derive(Clone, Debug)]
pub struct CastExpression {
	/// Destination signal class
	pub dest_class: Option<SignalClass>,

	/// Destination signal sensitivity
	pub dest_sensitivity: Option<SignalSensitivity>,

	/// Source expression
	pub src: Box<Expression>,
}

/// A binary expression
#[derive(Clone, Debug)]
pub struct BinaryExpression {
	/// Binary operator type
	pub op: BinaryOp,

	/// Left hand side expression
	pub lhs: Box<Expression>,

	/// Right hand side expression
	pub rhs: Box<Expression>,
}

/// A unary expression
#[derive(Clone, Debug)]
pub struct UnaryExpression {
	/// Unary operator type
	pub op: UnaryOp,

	/// Operand expression
	pub operand: Box<Expression>,
}

/// Language expression
#[derive(Clone, Debug)]
pub enum Expression {
	Conditional(ConditionalExpression),
	Constant(NumericConstant),
	Signal(SignalSlice),
	Binary(BinaryExpression),
	Builtin(BuiltinOp),
	Unary(UnaryExpression),
	Cast(CastExpression),
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

	/// Returns a conditional expression builder
	pub fn new_conditional(default: Expression) -> ConditionalExpressionBuilder {
		ConditionalExpressionBuilder::new(default)
	}

	/// Cassts expression to a different type
	pub fn cast(self, dest_class: Option<SignalClass>, dest_sensitivity: Option<SignalSensitivity>) -> Self {
		Self::Cast(CastExpression {
			dest_class,
			dest_sensitivity,
			src: Box::new(self),
		})
	}

	/// Performs zero extension
	pub fn zero_extend(self, width: Expression) -> Self {
		Self::Builtin(BuiltinOp::ZeroExtend {
			expr: Box::new(self),
			width: Box::new(width),
		})
	}

	/// Performs sign extension
	pub fn sign_extend(self, width: Expression) -> Self {
		Self::Builtin(BuiltinOp::SignExtend {
			expr: Box::new(self),
			width: Box::new(width),
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
			Self::Signal(slice) => Some(slice.clone()),
			// TODO index/range expression drive
			_ => None,
		}
	}

	// TODO reduction AND/OR/XOR
	// TODO bitwise not
	// TODO from bool
	// TODO remaining binary ops

	// Casts:
	// TODO from bool
}

/// Implements a conversion from signal ID to an expression
impl From<SignalId> for Expression {
	fn from(signal: SignalId) -> Self {
		Self::Signal(signal.into())
	}
}

impl From<NumericConstant> for Expression {
	fn from(constant: NumericConstant) -> Self {
		Self::Constant(constant)
	}
}

impl From<ConditionalExpression> for Expression {
	fn from(expr: ConditionalExpression) -> Self {
		Self::Conditional(expr)
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

impl From<u32> for Expression {
	fn from(value: u32) -> Self {
		Self::Constant(value.into())
	}
}

impl From<i32> for Expression {
	fn from(value: i32) -> Self {
		Self::Constant(value.into())
	}
}
