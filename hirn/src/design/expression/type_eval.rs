use super::{
	eval::EvalAssumptions, BinaryExpression, BuiltinOp, CastExpression, ConditionalExpression, EvalContext, EvalError,
	EvalType, EvaluatesType, NumericConstant, UnaryExpression,
};
use crate::design::{BinaryOp, Expression, SignalId, SignalSensitivity, SignalSignedness, SignalSlice};

impl EvaluatesType for NumericConstant {
	fn eval_type(&self, _ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		Ok(EvalType {
			signedness: self.signedness()?,
			sensitivity: SignalSensitivity::Generic,
		})
	}
}

impl EvaluatesType for SignalId {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		if let Some(design) = ctx.design() {
			let signal = design.get_signal(*self).expect("Evaluated signal must be in design");
			Ok(EvalType {
				signedness: signal.signedness(),
				sensitivity: signal.sensitivity().clone(),
			})
		}
		else {
			Err(EvalError::NoDesign)
		}
	}
}

impl EvaluatesType for BinaryExpression {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		let lhs = self.lhs.eval_type(ctx)?;
		let rhs = self.rhs.eval_type(ctx)?;

		if lhs.signedness != rhs.signedness {
			return Err(EvalError::MixedSignedness);
		}

		use BinaryOp::*;
		let signedness = match self.op {
			// Boolean results
			Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual | LogicalAnd | LogicalOr => {
				SignalSignedness::Unsigned
			},

			// Copy signedness from operands
			BitwiseAnd | BitwiseOr | BitwiseXor | Add | Subtract | Multiply | Divide | Modulo | Max | Min
			| ShiftLeft | ShiftRight => lhs.signedness,
		};

		Ok(EvalType {
			sensitivity: lhs
				.sensitivity
				.combine(&rhs.sensitivity)
				.ok_or(EvalError::InvalidSensitivityCombination)?,
			signedness,
		})
	}
}

impl EvaluatesType for SignalSlice {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		self.signal.eval_type(ctx)
	}
}

impl EvaluatesType for ConditionalExpression {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		let default_type = self.default_value().eval_type(ctx)?;
		let branch_types_res: Result<Vec<_>, EvalError> =
			self.branches().iter().map(|br| br.value().eval_type(ctx)).collect();
		let branch_types = branch_types_res?;

		let condition_types_res: Result<Vec<_>, EvalError> =
			self.branches().iter().map(|br| br.condition().eval_type(ctx)).collect();
		let condition_types = condition_types_res?;

		if !branch_types.iter().all(|t| t.signedness == default_type.signedness) {
			return Err(EvalError::MixedSignedness);
		}

		let mut sensitivity = default_type.sensitivity;
		for t in &branch_types {
			sensitivity = sensitivity.or_worse(&t.sensitivity);
		}

		for t in &condition_types {
			sensitivity = sensitivity.or_worse(&t.sensitivity);
		}

		Ok(EvalType {
			sensitivity,
			signedness: default_type.signedness,
		})
	}
}

impl EvaluatesType for UnaryExpression {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		let op_type = self.operand.eval_type(ctx)?;

		use SignalSensitivity::*;
		Ok(EvalType {
			signedness: op_type.signedness,
			sensitivity: match op_type.sensitivity {
				Comb(s) => Comb(s),
				Sync(s) => Comb(s),
				other => other,
			},
		})
	}
}

impl EvaluatesType for BuiltinOp {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		use BuiltinOp::*;
		Ok(match self {
			BitSelect { expr, .. } => expr.eval_type(ctx)?,
			BusSelect { expr, .. } => expr.eval_type(ctx)?,
			ZeroExtend { expr, .. } => EvalType {
				signedness: SignalSignedness::Unsigned,
				sensitivity: expr.eval_type(ctx)?.sensitivity,
			},
			SignExtend { expr, .. } => EvalType {
				signedness: SignalSignedness::Signed,
				sensitivity: expr.eval_type(ctx)?.sensitivity,
			},
			Replicate { expr, .. } => {
				let inner_type = expr.eval_type(ctx)?;
				EvalType {
					signedness: SignalSignedness::Unsigned,
					sensitivity: inner_type.sensitivity,
				}
			},
			Join(exprs) => {
				let mut sensitivity = SignalSensitivity::Generic;

				let types_res: Result<Vec<_>, EvalError> = exprs.iter().map(|e| e.eval_type(ctx)).collect();
				let types = types_res?;

				for t in &types {
					sensitivity = sensitivity.or_worse(&t.sensitivity);
				}

				EvalType {
					signedness: SignalSignedness::Unsigned,
					sensitivity,
				}
			},
			Width { .. } => EvalType {
				signedness: SignalSignedness::Unsigned,
				sensitivity: SignalSensitivity::Generic,
			},
		})
	}
}

impl EvaluatesType for CastExpression {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		let src_type = self.src.eval_type(ctx)?;
		let signedness = self.signedness.clone().unwrap_or(src_type.signedness);
		let sensitivity = self.sensitivity.clone().unwrap_or(src_type.sensitivity);

		Ok(EvalType {
			signedness,
			sensitivity,
		})
	}
}

impl EvaluatesType for Expression {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError> {
		use Expression::*;
		match self {
			Constant(value) => value.eval_type(ctx),
			Conditional(expr) => expr.eval_type(ctx),
			Binary(expr) => expr.eval_type(ctx),
			Unary(expr) => expr.eval_type(ctx),
			Cast(expr) => expr.eval_type(ctx),
			Builtin(builtin) => builtin.eval_type(ctx),
			Signal(slice) => slice.eval_type(ctx),
		}
	}
}
