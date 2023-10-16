use super::{
	eval::Evaluates, EvalContext, EvalError, Expression, NumericConstant, SignalId, SignalSlice, WidthExpression,
};
use super::{BinaryExpression, BinaryOp, BuiltinOp, CastExpression, ConditionalExpression, UnaryExpression, UnaryOp};

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
			LogicalAnd => match self.lhs.eval(ctx)?.is_nonzero() {
				true => Ok(self.rhs.eval(ctx)?),
				false => Ok(false.into()),
			},
			LogicalOr => match self.lhs.eval(ctx)?.is_nonzero() {
				true => Ok(true.into()),
				false => Ok(self.rhs.eval(ctx)?),
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
				}
				.into()
			},
		}
	}
}

impl Evaluates for UnaryExpression {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		use UnaryOp::*;
		let operand_value = self.operand.eval(ctx)?;
		match self.op {
			Negate => operand_value.op_neg(),
			LogicalNot => operand_value.op_lnot(),
			BitwiseNot => operand_value.op_bitwise_not(),
			ReductionAnd => operand_value.op_reduction_and(),
			ReductionOr => operand_value.op_reduction_or(),
			ReductionXor => operand_value.op_reduction_xor(),
		}
		.into()
	}
}

impl Evaluates for BuiltinOp {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError> {
		use BuiltinOp::*;
		match self {
			ZeroExtend { expr, width } => {
				let lhs = expr.eval(ctx)?;
				let width_value = width.eval(ctx)?;
				lhs.op_zext(width_value).into()
			},

			SignExtend { expr, width } => {
				let lhs = expr.eval(ctx)?;
				let width_value = width.eval(ctx)?;
				lhs.op_sext(width_value).into()
			},

			BusSelect { expr, msb, lsb } => {
				let lhs = expr.eval(ctx)?;
				let lsb_value = lsb.eval(ctx)?;
				let msb_value = msb.eval(ctx)?;
				lhs.op_bus_select(lsb_value, msb_value).into()
			},

			BitSelect { expr, index } => {
				let lhs = expr.eval(ctx)?;
				let rhs = index.eval(ctx)?;
				lhs.op_bit_select(rhs).into()
			},

			Replicate { expr, count } => {
				let lhs = expr.eval(ctx)?;
				let rhs = count.eval(ctx)?;
				lhs.op_replicate(rhs).into()
			},

			Width(arg) => match **arg {
				Expression::Signal(ref slice) => {
					// FIXME nasty cast
					let indices: Result<Vec<_>, EvalError> = slice.indices.iter().map(|e| e.eval(ctx)?.try_into_u64().map(|v| v as i64)).collect();
					match ctx.signal(slice.signal, &indices?) {
						Some(value) => NumericConstant::new(
							value.width()?.into(),
							crate::design::SignalSignedness::Unsigned,
							64
						),

						None => Err(EvalError::MissingAssumption(slice.signal))
					}
				},

				Expression::Constant(ref c) => {
					NumericConstant::new(
						c.width()?.into(),
						crate::design::SignalSignedness::Unsigned,
						64
					)
				},
				_ => arg.width()?.eval(ctx),
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
