use super::{Expression, eval::Evaluates, eval::EvaluatesType,  eval::EvalType, EvalContext, EvalError, NumericConstant, SignalSensitivity, eval::EvalResult};
use super::expression::{CastExpression, ConditionalExpression, BinaryExpression, UnaryExpression, BinaryOp, UnaryOp};

impl EvaluatesType for ConditionalExpression {
	fn eval_type(&self, ctx: &EvalContext) -> EvalResult<EvalType> {
		todo!();
	}
}

impl Evaluates for ConditionalExpression {
	fn eval(&self, ctx: &EvalContext) -> EvalResult<NumericConstant> {
		todo!();
	}
}

impl EvaluatesType for CastExpression {
	fn eval_type(&self, ctx: &EvalContext) -> EvalResult<EvalType> {
		todo!();
	}
}

impl Evaluates for CastExpression {
	fn eval(&self, ctx: &EvalContext) -> EvalResult<NumericConstant> {
		todo!();
	}
}

impl EvaluatesType for BinaryExpression {
	fn eval_type(&self, ctx: &EvalContext) -> EvalResult<EvalType> {
		todo!();
	}
}

impl Evaluates for BinaryExpression {
	fn eval(&self, ctx: &EvalContext) -> EvalResult<NumericConstant> {
		todo!();
	}
}

impl EvaluatesType for UnaryExpression {
	fn eval_type(&self, ctx: &EvalContext) -> EvalResult<EvalType> {
		todo!();
	}
}

impl Evaluates for UnaryExpression {
	fn eval(&self, ctx: &EvalContext) -> EvalResult<NumericConstant> {
		todo!();
	}
}

impl EvaluatesType for Expression {
	fn eval_type(&self, ctx: &EvalContext) -> EvalResult<EvalType> {
		todo!();
	}
}

impl Evaluates for Expression {
	fn eval(&self, ctx: &EvalContext) -> EvalResult<NumericConstant> {
		use Expression::*;
		use EvalResult::*;
		match self {
			Constant(value) => Ok(value.clone()),
			Conditional(expr) => expr.eval(ctx),
			Binary(expr) => expr.eval(ctx),
			Unary(expr) => expr.eval(ctx),
			Cast(expr) => expr.eval(ctx),
			Signal(id) => Err(EvalError::MissingAssumption(*id)), // FIXME
			Slice(slice) => Err(EvalError::MissingAssumption(slice.signal)), // FIXME
		}
	}
}

// TODO Evaluates for SignalSlice and SignalId
// TODO remove SignalId from expression i guess