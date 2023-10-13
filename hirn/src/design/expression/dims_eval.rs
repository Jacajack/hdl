use crate::design::{SignalId, SignalSlice, Expression};
use super::{EvaluatesDimensions, NumericConstant, EvalContext, EvalDims, EvalError, BinaryExpression, ConditionalExpression, UnaryExpression, BuiltinOp, CastExpression};

impl EvaluatesDimensions for NumericConstant {
	fn eval_dims(&self, _ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		Ok(EvalDims {
			width: self.width()?.into(),
			dimensions: vec![],
		})
	}
}

impl EvaluatesDimensions for SignalId {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		if let Some(design) = ctx.design() {
			let design = design.borrow();
			let _signal = design.get_signal(*self).unwrap();
			// let width = signal.class.width().eval(ctx)?.try_into_u64().unwrap(); // FIXME unwrap

			// TODO check the width I guess???

			todo!();

			// let mut dimensions = Vec::new();
			// for dim in &signal.dimensions {
			// 	match dim.eval(ctx) {
			// 		Ok(value) => dimensions.push(value.try_into_u64().unwrap()), // FIXME unwrap
			// 		Err(err) => return Err(err),
			// 	}
			// }

			// // TODO check the dimensions??

			// Ok(EvalDims { dimensions, width })
		}
		else {
			Err(EvalError::NoDesign)
		}
	}
}

impl EvaluatesDimensions for SignalSlice {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		if let Some(design) = ctx.design() {
			let design = design.borrow();
			let signal = design.get_signal(self.signal).unwrap();

			// Make sure we get a scalar!
			if signal.dimensions.len() != self.indices.len() {
				return Err(EvalError::InvalidIndexRank);
			}

			Ok(EvalDims {
				dimensions: vec![],
				width: self.signal.eval_dims(ctx)?.width,
			})
		}
		else {
			return Err(EvalError::NoDesign);
		}
	}
}

impl EvaluatesDimensions for BinaryExpression {
	fn eval_dims(&self, _ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		todo!();
	}
}

impl EvaluatesDimensions for ConditionalExpression {
	fn eval_dims(&self, _ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		todo!();
	}
}

impl EvaluatesDimensions for CastExpression {
	fn eval_dims(&self, _ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		todo!();
	}
}

impl EvaluatesDimensions for UnaryExpression {
	fn eval_dims(&self, _ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		todo!();
	}
}

impl EvaluatesDimensions for BuiltinOp {
	fn eval_dims(&self, _ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		todo!();
	}
}

impl EvaluatesDimensions for Expression {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		use Expression::*;
		match self {
			Constant(value) => value.eval_dims(ctx),
			Conditional(expr) => expr.eval_dims(ctx),
			Binary(expr) => expr.eval_dims(ctx),
			Unary(expr) => expr.eval_dims(ctx),
			Cast(expr) => expr.eval_dims(ctx),
			Signal(slice) => slice.eval_dims(ctx),
			Builtin(builtin) => builtin.eval_dims(ctx),
		}
	}
}