use super::{Expression, DesignHandle, SignalSensitivity, SignalId, NumericConstant};
use thiserror::Error;

pub struct EvalContext {
	design: Option<DesignHandle>,
	assumptions: Vec<(SignalId, NumericConstant)>,
}

pub trait Evaluates {
	/// Evaluate width of the expression (context required)
	fn width(&self, ctx: &EvalContext) -> Result<Expression, EvalError>;

	/// Evaluate sensitivity of the expression (context required)
	fn sensitivity(&self, ctx: &EvalContext) -> Result<SignalSensitivity, EvalError>;

	/// Evaluate the expression in the given context
	fn eval_in_context(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError>;

	/// Evaluate the expression without any context (for compile time constants & math)
	fn eval_constant(&self) -> Result<NumericConstant, EvalError>;
}

#[derive(Debug, Clone, Error)]
pub enum EvalError {
	#[error("Value of signal was not assumed, cannot evaluate")]
	MissingAssumption(SignalId),
}