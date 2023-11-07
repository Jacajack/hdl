mod eval;
mod expression_eval;
mod expression_rust_ops;
mod expression_validate;
mod narrow_eval;
mod numeric_constant;
mod type_eval;
mod width_expression;

pub use eval::{EvalAssumptions, EvalContext, EvalError, EvalType, Evaluates, EvaluatesType};
pub use expression_validate::ExpressionError;
pub use narrow_eval::NarrowEval;
pub use numeric_constant::NumericConstant;
pub use width_expression::WidthExpression;

use super::signal::{SignalSensitivity, SignalSlice, SignalSliceRange};
use super::{DesignHandle, SignalId, SignalSignedness};

/// Binary operators
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

impl BuiltinOp {
	pub fn transform<T>(&mut self, f: &dyn Fn(&mut Expression) -> Result<(), T>) -> Result<(), T> {
		use BuiltinOp::*;
		match self {
			ZeroExtend { expr, width } | SignExtend { expr, width } => {
				f(expr)?;
				expr.transform(f)?;
				f(width)?;
				width.transform(f)?;
			},

			BusSelect { expr, msb, lsb } => {
				f(expr)?;
				expr.transform(f)?;
				f(lsb)?;
				lsb.transform(f)?;
				f(msb)?;
				msb.transform(f)?;
			},

			BitSelect { expr, index } => {
				f(expr)?;
				expr.transform(f)?;
				f(index)?;
				index.transform(f)?;
			},

			Replicate { expr, count } => {
				f(expr)?;
				expr.transform(f)?;
				f(count)?;
				count.transform(f)?;
			},

			Join(exprs) => {
				for expr in exprs {
					f(expr)?;
					expr.transform(f)?;
				}
			},

			Width(expr) => {
				f(expr)?;
				expr.transform(f)?;
			},
		}
		Ok(())
	}

	pub fn traverse<T>(&self, f: &mut dyn FnMut(&Expression) -> Result<bool, T>) -> Result<(), T> {
		use BuiltinOp::*;
		match self {
			ZeroExtend { expr, width } | SignExtend { expr, width } => {
				if f(expr)? {
					expr.traverse(f)?;
				}
				if f(width)? {
					width.traverse(f)?;
				}
			},

			BusSelect { expr, msb, lsb } => {
				if f(expr)? {
					expr.traverse(f)?;
				}
				if f(lsb)? {
					lsb.traverse(f)?;
				}
				if f(msb)? {
					msb.traverse(f)?;
				}
			},

			BitSelect { expr, index } => {
				if f(expr)? {
					expr.traverse(f)?;
				}
				if f(index)? {
					index.traverse(f)?;
				}
			},

			Replicate { expr, count } => {
				if f(expr)? {
					expr.traverse(f)?;
				}
				if f(count)? {
					count.traverse(f)?;
				}
			},

			Join(exprs) => {
				for expr in exprs {
					if f(expr)? {
						expr.traverse(f)?;
					}
				}
			},

			Width(expr) => {
				if f(expr)? {
					expr.traverse(f)?;
				}
			},
		}
		Ok(())
	}
}

/// Represents a conditional expression branch
#[derive(Clone, Debug)]
pub struct ConditionalExpressionBranch {
	/// Condition expression
	condition: Expression,

	/// Value when condition is true
	value: Expression,
}

impl ConditionalExpressionBranch {
	pub fn condition(&self) -> &Expression {
		&self.condition
	}

	pub fn value(&self) -> &Expression {
		&self.value
	}
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

	pub fn branches(&self) -> &Vec<ConditionalExpressionBranch> {
		&self.branches
	}

	pub fn default_value(&self) -> &Expression {
		&self.default
	}

	fn add_branch(&mut self, condition: Expression, value: Expression) {
		self.branches.push(ConditionalExpressionBranch { condition, value });
	}

	pub fn transform<T>(&mut self, f: &dyn Fn(&mut Expression) -> Result<(), T>) -> Result<(), T> {
		f(&mut self.default)?;
		self.default.transform(f)?;
		for branch in &mut self.branches {
			f(&mut branch.condition)?;
			branch.condition.transform(f)?;
			f(&mut branch.value)?;
			branch.value.transform(f)?;
		}
		Ok(())
	}

	pub fn traverse<T>(&self, f: &mut dyn FnMut(&Expression) -> Result<bool, T>) -> Result<(), T> {
		if f(&self.default)? {
			self.default.traverse(f)?;
		}
		for branch in &self.branches {
			if f(&branch.condition)? {
				branch.condition.traverse(f)?;
			}
			if f(&branch.value)? {
				branch.value.traverse(f)?;
			}
		}

		Ok(())
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
	pub signedness: Option<SignalSignedness>,

	pub sensitivity: Option<SignalSensitivity>,

	/// Source expression
	pub src: Box<Expression>,
}

impl CastExpression {
	pub fn transform<T>(&mut self, f: &dyn Fn(&mut Expression) -> Result<(), T>) -> Result<(), T> {
		f(&mut self.src)?;
		self.src.transform(f)?;
		Ok(())
	}

	pub fn traverse<T>(&self, f: &mut dyn FnMut(&Expression) -> Result<bool, T>) -> Result<(), T> {
		if f(&self.src)? {
			self.src.traverse(f)?;
		}
		Ok(())
	}
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

impl BinaryExpression {
	pub fn transform<T>(&mut self, f: &dyn Fn(&mut Expression) -> Result<(), T>) -> Result<(), T> {
		f(&mut self.lhs)?;
		self.lhs.transform(f)?;
		f(&mut self.rhs)?;
		self.rhs.transform(f)?;
		Ok(())
	}

	pub fn traverse<T>(&self, f: &mut dyn FnMut(&Expression) -> Result<bool, T>) -> Result<(), T> {
		if f(&self.lhs)? {
			self.lhs.traverse(f)?;
		}
		if f(&self.rhs)? {
			self.rhs.traverse(f)?;
		}
		Ok(())
	}
}

/// A unary expression
#[derive(Clone, Debug)]
pub struct UnaryExpression {
	/// Unary operator type
	pub op: UnaryOp,

	/// Operand expression
	pub operand: Box<Expression>,
}

impl UnaryExpression {
	pub fn transform<T>(&mut self, f: &dyn Fn(&mut Expression) -> Result<(), T>) -> Result<(), T> {
		f(&mut self.operand)?;
		self.operand.transform(f)?;
		Ok(())
	}

	pub fn traverse<T>(&self, f: &mut dyn FnMut(&Expression) -> Result<bool, T>) -> Result<(), T> {
		if f(&self.operand)? {
			self.operand.traverse(f)?;
		}
		Ok(())
	}
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

	/// Returns a new join expression
	pub fn new_join(expressions: Vec<Expression>) -> Self {
		Self::Builtin(BuiltinOp::Join(expressions))
	}

	/// Returns a new replicate expression
	pub fn new_replicate(expr: Expression, count: Expression) -> Self {
		Self::Builtin(BuiltinOp::Replicate {
			expr: Box::new(expr),
			count: Box::new(count),
		})
	}

	/// Cassts expression to a different type
	pub fn cast(self, dest_signedness: Option<SignalSignedness>, dest_sensitivity: Option<SignalSensitivity>) -> Self {
		Self::Cast(CastExpression {
			signedness: dest_signedness,
			sensitivity: dest_sensitivity,
			src: Box::new(self),
		})
	}

	/// Casts to unsigned
	pub fn cast_unsigned(self) -> Self {
		self.cast(Some(SignalSignedness::Unsigned), None)
	}

	/// Casts to signed
	pub fn cast_signed(self) -> Self {
		self.cast(Some(SignalSignedness::Signed), None)
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

	/// Perform logical NOT on this expression
	pub fn logical_not(self) -> Self {
		Self::Unary(UnaryExpression {
			op: UnaryOp::LogicalNot,
			operand: Box::new(self),
		})
	}

	/// Get max of two expressions
	pub fn max(self, rhs: Expression) -> Self {
		Self::Binary(BinaryExpression {
			op: BinaryOp::Max,
			lhs: Box::new(self),
			rhs: Box::new(rhs),
		})
	}

	/// Get min of two expressions
	pub fn min(self, rhs: Expression) -> Self {
		Self::Binary(BinaryExpression {
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

	/// Selects one bit from the expression
	pub fn bit_select(self, n: Expression) -> Self {
		Self::Builtin(BuiltinOp::BitSelect {
			expr: Box::new(self),
			index: Box::new(n),
		})
	}

	/// Selects range of bits from the expression
	pub fn bus_select(self, msb: Expression, lsb: Expression) -> Self {
		Self::Builtin(BuiltinOp::BusSelect {
			expr: Box::new(self),
			msb: Box::new(msb),
			lsb: Box::new(lsb),
		})
	}

	/// Attempt to drive the expression if possible.
	/// Returns affected signal slice if drivable.
	pub fn try_drive(&self) -> Option<SignalSlice> {
		use Expression::*;
		match self {
			Signal(slice) => Some(slice.clone()),
			Builtin(BuiltinOp::BusSelect { expr, .. }) | Builtin(BuiltinOp::BitSelect { expr, .. }) => expr.try_drive(),
			_ => None,
		}
	}

	/// Returns slice and bits affected if this expression is driven
	pub fn try_drive_bits(&self) -> Option<SignalSliceRange> {
		use Expression::*;
		match self {
			Signal(slice) => Some(SignalSliceRange::new_full(slice.clone())),

			Builtin(BuiltinOp::BusSelect { expr, lsb, msb }) => {
				let inner = expr.try_drive_bits()?;
				match (inner.slice(), inner.lsb_msb()) {
					(inner, None) => Some(SignalSliceRange::new(inner.clone(), (**lsb).clone(), (**msb).clone())),
					(inner, Some((inner_lsb, _))) => {
						let new_lsb = inner_lsb.clone() + (**lsb).clone();
						let new_msb = inner_lsb.clone() + (**msb).clone();
						Some(SignalSliceRange::new(inner.clone(), new_lsb, new_msb))
					},
				}
			},

			Builtin(BuiltinOp::BitSelect { expr, index }) => {
				let inner = expr.try_drive_bits()?;
				match (inner.slice(), inner.lsb_msb()) {
					(inner, None) => Some(SignalSliceRange::new(
						inner.clone(),
						(**index).clone(),
						(**index).clone(),
					)),
					(inner, Some((inner_lsb, _))) => {
						let new_lsb = inner_lsb.clone() + (**index).clone();
						let new_msb = new_lsb.clone();
						Some(SignalSliceRange::new(inner.clone(), new_lsb, new_msb))
					},
				}
			},

			_ => None,
		}
	}

	/// Returns a list of signal slice ranges which are used in this expression
	pub fn get_used_slice_ranges(&self) -> Vec<SignalSliceRange> {
		let mut slices = vec![];
		self.traverse(&mut |expr| -> Result<bool, ()> {
			if let Some(slice) = expr.try_drive_bits() {
				// This Some(false) deserves a bit of explanation
				// if we encounter signal[0:15][0] we don't want to go deeper
				// after the bit select. Doing so would result in a duplicate
				// 'signal' references.
				slices.push(slice);
				Ok(false)
			}
			else {
				Ok(true)
			}
		})
		.unwrap();
		slices
	}

	pub fn transform<T>(&mut self, f: &dyn Fn(&mut Expression) -> Result<(), T>) -> Result<(), T> {
		f(self)?;
		use Expression::*;
		match self {
			Binary(expr) => expr.transform(f)?,
			Unary(expr) => expr.transform(f)?,
			Conditional(expr) => expr.transform(f)?,
			Builtin(expr) => expr.transform(f)?,
			Cast(expr) => expr.transform(f)?,
			Constant(_) => (),
			Signal(slice) => {
				for index_expr in &mut slice.indices {
					f(index_expr)?;
					index_expr.transform(f)?;
				}
			},
		}
		Ok(())
	}

	pub fn traverse<T>(&self, f: &mut dyn FnMut(&Expression) -> Result<bool, T>) -> Result<(), T> {
		if f(self)? {
			use Expression::*;
			match self {
				Binary(expr) => expr.traverse(f)?,
				Unary(expr) => expr.traverse(f)?,
				Conditional(expr) => expr.traverse(f)?,
				Builtin(expr) => expr.traverse(f)?,
				Cast(expr) => expr.traverse(f)?,
				Constant(_) => (),
				Signal(slice) => {
					for index_expr in &slice.indices {
						if f(index_expr)? {
							index_expr.traverse(f)?;
						}
					}
				},
			}
		}
		Ok(())
	}
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

impl From<bool> for Expression {
	fn from(value: bool) -> Self {
		Self::Constant(value.into())
	}
}
