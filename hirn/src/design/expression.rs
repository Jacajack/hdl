use super::expression_width::WidthExpression;
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
	Max,
	Min,
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
	BusSelect {
		expr: Box<Expression>,
		msb: Box<Expression>,
		lsb: Box<Expression>,
	},
	BitSelect {
		expr: Box<Expression>,
		index: Box<Expression>,
	},
	Replicate {
		expr: Box<Expression>,
		count: Box<Expression>,
	},
	Join(Vec<Expression>),
	Width(Box<Expression>),
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

	pub fn default_value(&self) -> &Expression {
		&self.default
	}

	fn add_branch(&mut self, condition: Expression, value: Expression) {
		self.branches.push(ConditionalExpressionBranch { condition, value });
	}
}

/// A helper class for constructing conditional/match expressions
#[derive(Clone, Debug)]
pub struct ConditionalExpressionBuilder {
	expr: ConditionalExpression,
}

impl ConditionalExpressionBuilder {
	pub fn new(default: Expression) -> Self {
		Self {
			expr: ConditionalExpression::new(default),
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
	pub dest_class: Option<SignalClass>, // FIXME we cannot cast width - this should be sensitivity + sign only

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
		// TODO rewrite as join(rep(width - this_width, 0u1), this)
		Self::Builtin(BuiltinOp::ZeroExtend {
			expr: Box::new(self),
			width: Box::new(width),
		})
	}

	/// Performs sign extension
	pub fn sign_extend(self, width: Expression) -> Self {
		// TODO rewrite as join(rep(width - this_width, this[this_width - 1]), this)
		Self::Builtin(BuiltinOp::SignExtend {
			expr: Box::new(self),
			width: Box::new(width),
		})
	}

	/// Get max of two expressions
	pub fn max(self, rhs: Expression) -> Self {
		// TODO rewrite as conditional?
		Self::Binary(BinaryExpression{
			op: BinaryOp::Max,
			lhs: Box::new(self),
			rhs: Box::new(rhs),
		})
	}

	/// Get min of two expressions
	pub fn min(self, rhs: Expression) -> Self {
		// TODO rewrite as conditional?
		Self::Binary(BinaryExpression{
			op: BinaryOp::Min,
			lhs: Box::new(self),
			rhs: Box::new(rhs),
		})
	}

	/// Join bits with another expression
	pub fn join(self, rhs: Expression) -> Self {
		Self::Builtin(BuiltinOp::Join(vec![self, rhs]))
	}

	/// Joins bits with provided expressions
	pub fn join_many(self, expressions: Vec<Expression>) -> Self {
		let mut list = vec![self];
		list.extend(expressions);
		Self::Builtin(BuiltinOp::Join(list))
	}

	/// Selects range of bits from the expression
	pub fn select_bits(self, msb: Expression, lsb: Expression) -> Self {
		Self::Builtin(BuiltinOp::BusSelect {
			expr: Box::new(self),
			msb: Box::new(msb),
			lsb: Box::new(lsb),
		})
	}

	/// Attempt to drive the expression if possible.
	/// Returns affected signal slice if drivable.
	/// TODO is it necessary to return a slice here?
	pub fn try_drive(&self) -> Option<SignalSlice> {
		match self {
			Self::Signal(slice) => Some(slice.clone()),
			// Self::Builtin(BuiltinOp::BusSelect { expr, msb, lsb }) => {
			// 	SingalSlice{

			// 	}
			// },

			// Self::Builtin(BuiltinOp::BitSelect { expr, index }) => {

			// }
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

impl From<SignalSlice> for Expression {
	fn from(slice: SignalSlice) -> Self {
		Self::Signal(slice)
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

impl From<BuiltinOp> for Expression {
	fn from(expr: BuiltinOp) -> Self {
		Self::Builtin(expr)
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
