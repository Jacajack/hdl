use super::eval::EvaluatesDimensions;
use super::{Expression, eval::Evaluates, eval::EvaluatesType,  eval::EvalType, EvalContext, EvalError, NumericConstant, SignalSensitivity, eval::EvalResult, eval::EvalDims, SignalId};
use super::expression::{CastExpression, ConditionalExpression, BinaryExpression, UnaryExpression, BinaryOp, UnaryOp};

impl EvaluatesType for NumericConstant {
	fn eval_type(&self, _ctx: &EvalContext) -> Result<EvalType, EvalError> {
		Ok(EvalType {
			signedness: self.signedness(),
			sensitivity: SignalSensitivity::Const,
		})
	}
}

impl EvaluatesDimensions for NumericConstant {
	fn eval_dims(&self, _ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		Ok(EvalDims{
			width: self.width(),
			dimensions: vec![],
		})
	}
}

impl EvaluatesDimensions for SignalId {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		if let Some(design) = ctx.design() {
			let design = design.borrow();
			let signal = design.get_signal(*self).unwrap();
			let width = signal.class.width.eval(ctx)?.try_into_u64().unwrap(); // FIXME unwrap

			// TODO check the width I guess???

			let mut dimensions = Vec::new();
			for dim in &signal.dimensions {
				match dim.eval(ctx) {
					Ok(value) => dimensions.push(value.try_into_u64().unwrap()), // FIXME unwrap
					Err(err) => return Err(err),
				}
			}

			// TODO check the dimensions??

			Ok(EvalDims {
				dimensions,
				width,
			})
		}
		else {
			Err(EvalError::NoDesign)
			
		}
	}
}

impl EvaluatesType for SignalId {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		if let Some(design) = ctx.design() {
			let design = design.borrow();
			let signal = design.get_signal(*self).unwrap();
			Ok(EvalType {
				signedness: signal.class.signedness,
				sensitivity: signal.sensitivity.clone(),
			})
		}
		else {
			Err(EvalError::NoDesign)
		}
	}
}

impl EvaluatesType for ConditionalExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		todo!();
	}
}

impl Evaluates for ConditionalExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl EvaluatesType for CastExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		todo!();
	}
}

impl Evaluates for CastExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl EvaluatesType for BinaryExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		todo!();
	}
}

impl Evaluates for BinaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl EvaluatesType for UnaryExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		todo!();
	}
}

impl Evaluates for UnaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl EvaluatesType for Expression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		use Expression::*;
		match self {
			Constant(value) => value.eval_type(ctx),
			Conditional(expr) => expr.eval_type(ctx),
			Binary(expr) => expr.eval_type(ctx),
			Unary(expr) => expr.eval_type(ctx),
			Cast(expr) => expr.eval_type(ctx),
			Signal(id) => id.eval_type(ctx),
			Slice(slice) => todo!(), // FIXME
		}
	}
}

impl Evaluates for Expression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		use Expression::*;
		match self {
			Constant(value) => Ok(value.clone()),
			Conditional(expr) => expr.eval(ctx),
			Binary(expr) => expr.eval(ctx),
			Unary(expr) => expr.eval(ctx),
			Cast(expr) => expr.eval(ctx),
			Signal(id) => todo!(), // FIXME
			Slice(slice) => todo!(), // FIXME
		}
	}
}

// TODO Evaluates for SignalSlice and SignalId
// TODO remove SignalId from expression i guess