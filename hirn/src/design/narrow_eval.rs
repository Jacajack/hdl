use crate::BinaryOp;
use super::{EvalContext, Expression, EvalError, expression::{BinaryExpression, BuiltinOp}};
use super::expression_width::WidthExpression;

pub trait NarrowEval {
	fn narrow_eval(&self, ctx: &EvalContext) -> Result<i64, EvalError>;

	fn const_narrow_eval(&self) -> Result<i64, EvalError> {
		self.narrow_eval(&EvalContext::default())
	}
}

impl NarrowEval for BinaryExpression {
	fn narrow_eval(&self, ctx: &EvalContext) -> Result<i64, EvalError> {
		use BinaryOp::*;
		let lhs = self.lhs.const_narrow_eval()?;
		let rhs = self.rhs.const_narrow_eval()?;

		match self.op {
			Add => Ok(lhs + rhs),
			Subtract => Ok(lhs - rhs),
			Multiply => Ok(lhs * rhs),
			Min => Ok(lhs.min(rhs)),
			Max => Ok(lhs.max(rhs)),
			_ => Err(EvalError::NarrowEvalNotSupported),
		}
	}
}

impl NarrowEval for BuiltinOp {
	fn narrow_eval(&self, ctx: &EvalContext) -> Result<i64, EvalError> {
		use BuiltinOp::*;
		use Expression::*;
		match self {
			Width(expr) => {
				match **expr {
					Signal(slice) =>
						Ok(ctx.scalar_signal(slice.signal).ok_or(EvalError::MissingAssumption(slice.signal))?.width() as i64),
					Constant(nc) => Ok(nc.width() as i64),
					_ => expr.width().narrow_eval(ctx),
				}
			}
			_ => Err(EvalError::NarrowEvalNotSupported)
		}
	}
}

impl NarrowEval for Expression {
	fn narrow_eval(&self, ctx: &EvalContext) -> Result<i64, EvalError> {
		use Expression::*;

		match self {
			Constant(nc) => nc.try_into_i64().ok_or(EvalError::NarrowEvalRange),
			Binary(expr) => expr.narrow_eval(ctx),
			_ => Err(EvalError::NarrowEvalNotSupported),
		}
	}
}