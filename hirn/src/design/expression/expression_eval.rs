use super::eval::EvaluatesDimensions;
use super::{
	BinaryExpression, BinaryOp, BuiltinOp, CastExpression, ConditionalExpression, UnaryExpression, UnaryOp,
};
use super::{
	eval::EvalDims, eval::EvalType, eval::Evaluates, eval::EvaluatesType, EvalContext, EvalError,
	Expression, NumericConstant, SignalId, SignalSensitivity, SignalSlice, WidthExpression
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
		for branch in &self.branches {
			if branch.condition.eval(ctx)?.is_nonzero() {
				return branch.value.eval(ctx);
			}
		}
		self.default.eval(ctx)
	}
}

impl Evaluates for CastExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		self.src.eval(ctx)
	}
}

impl Evaluates for BinaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		use BinaryOp::*;
		match self.op {
			LogicalAnd => {
				match self.lhs.eval(ctx)?.is_nonzero() {
					true => Ok(self.rhs.eval(ctx)?),
					false => Ok(false.into()),
				}
			},
			LogicalOr => {
				match self.lhs.eval(ctx)?.is_nonzero() {
					true => Ok(true.into()),
					false => Ok(self.rhs.eval(ctx)?),
				}
			},

			_ => {
				let lhs = self.lhs.eval(ctx)?;
				let rhs = self.rhs.eval(ctx)?;
				match self.op {
					Add => lhs + rhs,
					Subtract => lhs - rhs,
					Multiply => lhs * rhs,
					Divide => lhs / rhs,
					Modulo => lhs % rhs,
					BitwiseAnd => lhs & rhs,
					BitwiseOr => lhs | rhs,
					BitwiseXor => lhs ^ rhs,
					ShiftLeft => lhs << rhs,
					ShiftRight => lhs >> rhs,
					Equal => lhs.op_eq(&rhs),
					NotEqual => lhs.op_ne(&rhs),
					Less => lhs.op_lt(&rhs),
					LessEqual => lhs.op_lte(&rhs),
					Greater => lhs.op_gt(&rhs),
					GreaterEqual => lhs.op_gte(&rhs),
					Max => lhs.op_max(&rhs),
					Min => lhs.op_min(&rhs),
					_ => unreachable!(),	
				}.into()
			}
		}
	}
}

impl Evaluates for UnaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		use UnaryOp::*;
		let operand_value = self.operand.eval(ctx)?;
		match self.op {
			Negate => todo!(), //Ok(operand_value.op_neg()),
			LogicalNot => operand_value.op_lnot(),
			BitwiseNot => todo!(), //Ok(operand_value.op_bitwise_not()),
			ReductionAnd => operand_value.op_reduction_and(),
			ReductionOr => operand_value.op_reduction_or(),
			ReductionXor => operand_value.op_reduction_xor(),
		}.into()
	}
}

impl Evaluates for BuiltinOp {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		use BuiltinOp::*;
		match self {
			ZeroExtend{expr, width} => {
				todo!();
			},

			SignExtend{expr, width} => {
				todo!();
			},

			BusSelect{expr, msb, lsb} => {
				todo!();
			},

			BitSelect{expr, index} => {
				todo!();
			},

			Replicate{expr, count} => {
				let lhs = expr.eval(ctx)?;
				let rhs = count.eval(ctx)?;
				lhs.op_replicate(rhs).into()
			},

			Width(expr) => {
				expr.width()?.eval(ctx)
			},

			Join(exprs) => {
				let args: Result<Vec<_>, EvalError> = exprs.iter().map(|e| e.eval(ctx)).collect();
				NumericConstant::join(args?).into()
			},
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

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_basic_eval() {
		let expr = Expression::from(5) + Expression::from(2) * 3.into();
		assert_eq!(expr.const_eval().unwrap(), 11.into());
	}
}