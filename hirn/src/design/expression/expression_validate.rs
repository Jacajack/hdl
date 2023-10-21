use std::{collections::HashSet, rc::Rc, cell::RefCell};

use thiserror::Error;

use crate::design::{SignalId, ScopeHandle, SignalSensitivity, SignalSlice, SignalSignedness, UnaryOp};

use super::{Expression, EvalContext, UnaryExpression, BinaryExpression, BuiltinOp, CastExpression, ConditionalExpression, EvaluatesType, EvalError, WidthExpression, NarrowEval};

#[derive(Clone, Debug, Copy, Error)]
pub enum ExpressionError {
	#[error("Signedness mismatch in the expression")]
	SignednessMismatch,
	
	#[error("Width mismatch in the expression")]
	WidthMismatch,
	
	#[error("Expression references signal not accessible within the specified scope")]
	ScopeError,

	#[error("Different branches in conditional expression have different types")]
	ConditionalBranchMismatch,

	#[error("Cannot cast expression to generic sensitivity")]
	GenericCast,

	#[error("Slice indices must be unsigned values")]
	SignedSliceIndex,

	#[error("Non-boolean condition")]
	NonBooleanCondition,

	#[error("Branch type mismatch")]
	BranchTypeMismatch,

	#[error("Logical operators require boolean operands")]
	NonBooleanLogic,

	#[error("Unsigned numeric negation")]
	UnsignedNegation,

	#[error("Bitwise operators require unsigned operands")]
	SignedBitwise,

}

impl UnaryExpression {
	fn shallow_validate(&self, ctx: &EvalContext, _scope: &ScopeHandle) -> Result<(), EvalError> {
		let expr_type = self.operand.eval_type(ctx)?;
		let expr_width = self.operand.width()?.narrow_eval(ctx).ok();

		use UnaryOp::*;
		match self.op {
			// Unary negation (-) operator requires signed operand
			Negate => {
				if expr_type.is_unsigned() {
					return Err(ExpressionError::UnsignedNegation.into());
				}
			},

			// Logical NOT operator requires a boolean operand
			LogicalNot => {
				match (expr_type.is_unsigned(), expr_width) {
					(true, Some(1)) => {}, // cool
					(true, None) => {}, // you can't always get what you want
					(true, Some(_)) | (false, _) => {return Err(ExpressionError::NonBooleanLogic.into());}, 
				}
			},

			// Bitwise NOT operator requires an unsigned operand
			BitwiseNot => {
				if expr_type.is_signed() {
					return Err(ExpressionError::SignedBitwise.into());
				}
			},

			// no checks for these guys
			ReductionAnd | ReductionOr | ReductionXor => {}, 
		}

		Ok(())
	}
}

impl BinaryExpression {
	fn shallow_validate(&self, _ctx: &EvalContext, _scope: &ScopeHandle) -> Result<(), EvalError> {
		Ok(()) // FIXME
	}
}

impl BuiltinOp {
	fn shallow_validate(&self, _ctx: &EvalContext, _scope: &ScopeHandle) -> Result<(), EvalError> {
		Ok(()) // FIXME
	}
}

impl CastExpression {
	fn shallow_validate(&self, _ctx: &EvalContext, _scope: &ScopeHandle) -> Result<(), EvalError> {
		// We must ensre that nothing is ever casted to generic sensitivity
		// Note: this could be conditionally allowed for expressions which already
		// are generic
		use SignalSensitivity::*;
		match self.sensitivity {
			Some(Generic) => Err(ExpressionError::GenericCast.into()),
			_ => Ok(()),
		}
	}
}

impl ConditionalExpression {
	fn shallow_validate(&self, ctx: &EvalContext, scope: &ScopeHandle) -> Result<(), EvalError> {
		let result_type = self.default_value().eval_type(ctx)?;
		let mut result_width = self.default_value().width()?.narrow_eval(ctx).ok();

		for b in self.branches() {
			let branch_type = b.value().eval_type(ctx)?;
			let branch_width = b.value().width()?.narrow_eval(ctx).ok();

			if branch_type.signedness != result_type.signedness {
				return Err(ExpressionError::BranchTypeMismatch.into());
			}

			match (result_width, branch_width) {
				// We found a mismatch
				(Some(rw), Some(bw)) => {
					if rw != bw {
						return Err(ExpressionError::WidthMismatch.into());
					}
				},

				// We've just found first width of a branch value
				(None, Some(bw)) => {
					result_width = Some(bw);
				},

				(None, None) | (Some(_), None) => {},
			}
		}

		// All condition expressions must be boolean (i.e. 1-bit unsigned)
		for b in self.branches() {
			let cond_type = b.condition().eval_type(ctx)?;
			
			let cond_width = 
				b.condition()
				.width()?
				.narrow_eval(ctx);

			use SignalSignedness::*;
			match (cond_type.signedness, cond_width) {
				(Signed, _) => return Err(ExpressionError::NonBooleanCondition.into()),
				(Unsigned, Ok(1)) => {},
				(Unsigned, Err(_)) => {}, // could not evaluate width but whatcha gonna do (it's probably generic)
				(_, Ok(_)) => return Err(ExpressionError::NonBooleanCondition.into())
			}
		}

		Ok(())
	}
}

fn shallow_validate_slice(slice: &SignalSlice, ctx: &EvalContext, scope: &ScopeHandle) -> Result<(), EvalError> {
	let design_handle = scope.design();
	let design = design_handle.borrow();
	let signal = design.get_signal(slice.signal).expect("Signal not in design");

	for index in &slice.indices {
		// let index_type = index.eval_type(ctx)?;
		// if index_type.signedness.is_signed() {
		// 	return Err(ExpressionError::SignedSliceIndex);
		// }

		// TODO indices must be generic
	}

	Ok(())
}

impl Expression {
	/// Returns a set of variables used in the expression
	pub fn get_variables(&self) -> HashSet<SignalId> {
		let vars = Rc::new(RefCell::new(HashSet::new()));
		self.traverse(&|e| -> Result<(), ()> {
			match e {
				Expression::Signal(slice) => {
					(*vars).borrow_mut().insert(slice.signal);
				},
				_ => {},
			};
			Ok(())
		}).unwrap();
		vars.take()
	}

	/// Validates the expression
	pub fn validate(&self, ctx: &EvalContext, scope: &ScopeHandle) -> Result<(), EvalError> {
		self.traverse(&|e| -> Result<(), EvalError> {
			// Check if all variables used in the expression are a subset of the ones
			// accessible within this scope
			let expr_variables = self.get_variables();
			let scope_variables = scope.visible_signals();
			if !expr_variables.is_subset(&scope_variables) {
				return Err(ExpressionError::ScopeError.into())
			}

			/// Peform shallow validation of each expresion sub-type (i.e. non-recursive)
			use Expression::*;
			match e {
				Constant(_) => Ok(()),
				Signal(slice) => shallow_validate_slice(slice, ctx, scope),
				Unary(e) => e.shallow_validate(ctx, scope),
				Binary(e) => e.shallow_validate(ctx, scope),
				Builtin(op) => op.shallow_validate(ctx, scope),
				Cast(c) => c.shallow_validate(ctx, scope),
				Conditional(c) => c.shallow_validate(ctx, scope),
			}
		})
	}

	/// Validate the expression without making any assumptions regarding values of signals
	pub fn validate_no_assumptions(&self, scope: &ScopeHandle) -> Result<(), EvalError> {
		self.validate(
			&EvalContext::without_assumptions(scope.design()), 
			scope
		)
	}
}

#[cfg(test)]
mod test {
	use crate::design::{Design, DesignError};

	#[test]
	fn basic_scope_rule_test() -> Result<(), DesignError> {
		let mut d = Design::new();
		let m1 = d.new_module("m1")?;
		let m2 = d.new_module("m2")?;

		let sig_foo = m1.scope().new_signal("foo")?
			.generic()
			.unsigned(16u32.into())
			.build()?;

		let sig_bar = m2.scope().new_signal("bar")?
			.generic()
			.unsigned(16u32.into())
			.build()?;
		
		// Should fail cause bar is not accessible in m1
		let assign_result = m1.scope().assign(sig_foo.into(), sig_bar.into());
		assert!(assign_result.is_err());
		Ok(())
	}
}