use super::eval::EvaluatesDimensions;
use super::{Expression, eval::Evaluates, eval::EvaluatesType,  eval::EvalType, EvalContext, EvalError, NumericConstant, SignalSensitivity, eval::EvalResult, eval::EvalDims, SignalId};
use super::expression::{CastExpression, ConditionalExpression, BinaryExpression, UnaryExpression, BinaryOp, UnaryOp, BuiltinOp};

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

impl Evaluates for NumericConstant {
	fn eval(&self, _ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		Ok(self.clone())
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

impl Evaluates for SignalId {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		// TODO check if the assumed width matches the signal width
		// TODO ah the fucking arrays i forgot about that


		ctx.signal_value(*self)
			.map(|v| v.clone())
			.ok_or(EvalError::MissingAssumption(*self))
	}
}

impl EvaluatesType for ConditionalExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		todo!();
	}
}

impl EvaluatesDimensions for ConditionalExpression {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
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

impl EvaluatesDimensions for CastExpression {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
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

impl EvaluatesDimensions for BinaryExpression {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		todo!();
	}
}

impl Evaluates for BinaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		let lhs = self.lhs.eval(ctx)?;
		let rhs = self.rhs.eval(ctx)?;

		use BinaryOp::*;
		match self.op {
			Add => lhs + rhs,
			// TODO remainig ops
			_ => todo!()
		}.into()
	}
}

impl EvaluatesType for UnaryExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		todo!();
	}
}

impl EvaluatesDimensions for UnaryExpression {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		todo!();
	}
}

impl Evaluates for UnaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl EvaluatesType for BuiltinOp {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		todo!();
	}
}

impl EvaluatesDimensions for BuiltinOp {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError> {
		todo!();
	}
}

impl Evaluates for BuiltinOp {
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
			Builtin(builtin) => builtin.eval_type(ctx),
			Signal(signal) => signal.eval_type(ctx),
		}
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
			Signal(signal) => signal.eval_dims(ctx),
			Builtin(builtin) => builtin.eval_dims(ctx),
		}
	}
}

impl Evaluates for Expression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		use Expression::*;
		match self {
			Constant(value) => value.eval(ctx),
			Conditional(expr) => expr.eval(ctx),
			Binary(expr) => expr.eval(ctx),
			Unary(expr) => expr.eval(ctx),
			Cast(expr) => expr.eval(ctx),
			Signal(signal) => signal.eval(ctx),
			Builtin(builtin) => builtin.eval(ctx),
		}
	}
}
