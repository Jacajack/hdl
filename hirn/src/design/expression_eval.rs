use super::{Expression, eval::Evaluates, EvalContext, EvalError, NumericConstant, SignalSensitivity};
use super::expression::{CastExpression, ConditionalExpression, BinaryExpression, UnaryExpression};

impl Evaluates for ConditionalExpression {
	fn width(&self, ctx: &EvalContext) -> Result<Expression, EvalError> {
		todo!();
	}

	fn sensitivity(&self, ctx: &EvalContext) -> Result<SignalSensitivity, EvalError> {
		todo!();
	}

	fn eval_in_context(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}

	fn eval_constant(&self) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl Evaluates for CastExpression {
	fn width(&self, ctx: &EvalContext) -> Result<Expression, EvalError> {
		todo!();
	}

	fn sensitivity(&self, ctx: &EvalContext) -> Result<SignalSensitivity, EvalError> {
		todo!();
	}

	fn eval_in_context(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}

	fn eval_constant(&self) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl Evaluates for BinaryExpression {
	fn width(&self, ctx: &EvalContext) -> Result<Expression, EvalError> {
		todo!();
	}

	fn sensitivity(&self, ctx: &EvalContext) -> Result<SignalSensitivity, EvalError> {
		todo!();
	}

	fn eval_in_context(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}

	fn eval_constant(&self) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl Evaluates for UnaryExpression {
	fn width(&self, ctx: &EvalContext) -> Result<Expression, EvalError> {
		todo!();
	}

	fn sensitivity(&self, ctx: &EvalContext) -> Result<SignalSensitivity, EvalError> {
		todo!();
	}

	fn eval_in_context(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}

	fn eval_constant(&self) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl Evaluates for Expression {
	fn width(&self, ctx: &EvalContext) -> Result<Expression, EvalError> {
		todo!();
	}

	fn sensitivity(&self, ctx: &EvalContext) -> Result<SignalSensitivity, EvalError> {
		todo!();
	}

	fn eval_in_context(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}

	fn eval_constant(&self) -> Result<NumericConstant, EvalError> {
		use Expression::*;
		match self {
			Constant(value) => Ok(value.clone()),
			Conditional(expr) => expr.eval_constant(),
			Binary(expr) => expr.eval_constant(),
			Unary(expr) => expr.eval_constant(),
			Cast(expr) => expr.eval_constant(),
			Signal(id) => Err(EvalError::MissingAssumption(*id)),
			Slice(slice) => Err(EvalError::MissingAssumption(slice.signal)),
		}
	}
}