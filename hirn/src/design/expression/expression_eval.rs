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
					Add => Ok(lhs + rhs),
					Subtract => Ok(lhs - rhs),
					Multiply => Ok(lhs * rhs),
					Divide => Ok(lhs / rhs),
					Modulo => Ok(lhs % rhs),
					BitwiseAnd => Ok(lhs & rhs),
					BitwiseOr => Ok(lhs | rhs),
					BitwiseXor => Ok(lhs ^ rhs),
					ShiftLeft => Ok(lhs << rhs),
					ShiftRight => Ok(lhs >> rhs),
					Equal => Ok(lhs.op_eq(&rhs)),
					NotEqual => Ok(lhs.op_ne(&rhs)),
					Less => Ok(lhs.op_lt(&rhs)),
					LessEqual => Ok(lhs.op_lte(&rhs)),
					Greater => Ok(lhs.op_gt(&rhs)),
					GreaterEqual => Ok(lhs.op_gte(&rhs)),
					Max => Ok(lhs.op_max(&rhs)),
					Min => Ok(lhs.op_min(&rhs)),
					_ => unreachable!(),	
				}
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
			LogicalNot => Ok(operand_value.op_lnot()),
			BitwiseNot => todo!(), //Ok(operand_value.op_bitwise_not()),
			ReductionAnd => Ok(operand_value.op_reduction_and()),
			ReductionOr => Ok(operand_value.op_reduction_or()),
			ReductionXor => Ok(operand_value.op_reduction_xor()),
		}
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
				todo!();
			},

			Width(expr) => {
				expr.width()?.eval(ctx)
			},

			Join(exprs) => {
				todo!();
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