use super::eval::EvaluatesDimensions;
use super::{
	BinaryExpression, BinaryOp, BuiltinOp, CastExpression, ConditionalExpression, UnaryExpression, UnaryOp,
};
use super::{
	eval::EvalDims, eval::EvalType, eval::Evaluates, eval::EvaluatesType, EvalContext, EvalError,
	Expression, NumericConstant, SignalId, SignalSensitivity, SignalSlice,
};

impl Evaluates for NumericConstant {
	fn eval(&self, _ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		Ok(self.clone())
	}
}

impl Evaluates for SignalId {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		if let Some(design) = ctx.design() {
			let design = design.borrow();
			let signal = design.get_signal(*self).unwrap();

			if !signal.is_scalar() {
				return Err(EvalError::NonScalar);
			}
		}
		else {
			return Err(EvalError::NoDesign);
		}

		ctx.scalar_signal(*self)
			.map(|v| v.clone())
			.ok_or(EvalError::MissingAssumption(*self))
	}
}

impl Evaluates for SignalSlice {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		// Evaluate indices
		let mut indices = Vec::new();
		for index in &self.indices {
			indices.push(index.eval(ctx)?.try_into_i64().or(Err(EvalError::InvalidIndex))?);
		}

		ctx.signal(self.signal, &indices)
			.map(|v| v.clone())
			.ok_or(EvalError::MissingAssumption(self.signal))
	}
}

impl Evaluates for ConditionalExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl Evaluates for CastExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl Evaluates for BinaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		let lhs = self.lhs.eval(ctx)?;
		let rhs = self.rhs.eval(ctx)?;

		use BinaryOp::*;
		todo!();
		// match self.op {
		// 	Add => lhs + rhs,
		// 	// TODO remainig ops
		// 	_ => todo!(),
		// }
		// .into()
	}
}

impl Evaluates for UnaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
	}
}

impl Evaluates for BuiltinOp {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		todo!();
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
