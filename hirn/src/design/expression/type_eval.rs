use crate::design::{SignalSensitivity, SignalId, SignalSlice, Expression, SignalSignedness};
use super::{EvaluatesType, NumericConstant, EvalContext, EvalType, EvalError, BinaryExpression, ConditionalExpression, UnaryExpression, BuiltinOp, BinaryOp, UnaryOp, CastExpression};

impl EvaluatesType for NumericConstant {
	fn eval_type(&self, _ctx: &EvalContext) -> Result<EvalType, EvalError> {
		Ok(EvalType {
			signedness: self.signedness()?,
			sensitivity: SignalSensitivity::Generic,
		})
	}
}

impl EvaluatesType for SignalId {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		if let Some(design) = ctx.design() {
			let design = design.borrow();
			let signal = design.get_signal(*self).unwrap();
			Ok(EvalType {
				signedness: signal.class.signedness(),
				sensitivity: signal.sensitivity.clone(),
			})
		}
		else {
			Err(EvalError::NoDesign)
		}
	}
}

impl EvaluatesType for BinaryExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		let lhs = self.lhs.eval_type(ctx)?;
		let rhs = self.rhs.eval_type(ctx)?;
		
		if lhs.signedness != rhs.signedness {
			return Err(EvalError::MixedSignedness);
		}

		Ok(EvalType {
			sensitivity: lhs.sensitivity.combine(&rhs.sensitivity)
				.ok_or(EvalError::InvalidSensitivityCombination)?,
			signedness: lhs.signedness,
		})
	}
}

impl EvaluatesType for SignalSlice {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		self.signal.eval_type(ctx)
	}
}

impl EvaluatesType for ConditionalExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		let default_type = self.default_value().eval_type(ctx)?;
		let branch_types_res: Result<Vec<_>, EvalError> = self.branches().iter().map(|br|{
			br.value().eval_type(ctx)
		}).collect();
		let branch_types = branch_types_res?;

		let condition_types_res: Result<Vec<_>, EvalError> = self.branches().iter().map(|br|{
			br.condition().eval_type(ctx)
		}).collect();
		let condition_types = condition_types_res?;

		if !branch_types.iter().all(|t| {t.signedness == default_type.signedness}) {
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
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		let op_type = self.operand.eval_type(ctx)?;

		use SignalSensitivity::*;
		Ok(EvalType {
			signedness: op_type.signedness,
			sensitivity: match op_type.sensitivity {
				Async => Async,
				Comb(s) => Comb(s),
				Sync(s) => Sync(s),
				Clock => Clock,
				Const => Const,
				Generic => Generic,
			}
		})
	}
}

impl EvaluatesType for BuiltinOp {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		use BuiltinOp::*;
		Ok(match self { 
			ZeroExtend{expr, ..} => expr.eval_type(ctx)?,
			SignExtend{expr, ..} => expr.eval_type(ctx)?,
			BusSelect{expr, ..} => expr.eval_type(ctx)?,
			BitSelect{expr, ..} => expr.eval_type(ctx)?,
			Replicate{expr, ..} => expr.eval_type(ctx)?,
			Join(exprs) => {
				let mut sensitivity = SignalSensitivity::Generic;

				let types_res: Result<Vec<_>, EvalError> = exprs.iter().map(|e|{
					e.eval_type(ctx)
				}).collect();
				let types = types_res?;

				for t in &types {
					sensitivity = sensitivity.or_worse(&t.sensitivity);
				}

				EvalType{
					signedness: SignalSignedness::Unsigned,
					sensitivity,
				}
			},
			Width{..} => EvalType {
				signedness: SignalSignedness::Unsigned,
				sensitivity: SignalSensitivity::Generic,
			},
		})
	}
}

impl EvaluatesType for CastExpression {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError> {
		let src_type = self.src.eval_type(ctx)?;
		let signedness = self.dest_class.clone()
			.map_or(src_type.signedness, |c| c.signedness());

		let sensitivity = self.dest_sensitivity
			.clone()
			.or(Some(src_type.sensitivity))
			.unwrap(); // FIXME

		Ok(EvalType{
			signedness,
			sensitivity,
		})
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
			Signal(slice) => slice.eval_type(ctx),
		}
	}
}