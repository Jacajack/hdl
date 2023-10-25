use std::{cell::RefCell, collections::HashSet, rc::Rc};

use thiserror::Error;

use crate::design::{
	BinaryOp, HasSensitivity, HasSignedness, ScopeHandle, SignalId, SignalSensitivity, SignalSignedness, SignalSlice,
	UnaryOp,
};

use super::{
	BinaryExpression, BuiltinOp, CastExpression, ConditionalExpression, EvalContext, EvalError, EvaluatesType,
	Expression, NarrowEval, UnaryExpression, WidthExpression,
};

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

	#[error("Non-boolean condition")]
	NonBooleanCondition,

	#[error("Branch type mismatch")]
	BranchTypeMismatch,

	#[error("Logical operators require boolean operands")]
	NonBooleanLogic,

	#[error("Unsigned numeric negation")]
	UnsignedNegation,

	#[error("Mixed signedness in arithmetic")]
	MixedSignedness,

	#[error("Bit shift requires unsigned RHS operand")]
	SignedShiftWidth,

	#[error("Slice indices must be generic")]
	VariableSliceIndex,

	#[error("Signal array has different rank than the slice reffering to it")]
	SliceRankMismatch,

	#[error("Empty join list")]
	EmptyJoinList,

	#[error("Replication count must be generic and nonzero")]
	InvalidReplicationCount,

	#[error("Extension width must be generic and nonzero")]
	InvalidExtensionWidth,

	#[error("Invalid bit index")]
	InvalidBitIndex,
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
					(true, None) => {},    // you can't always get what you want
					(true, Some(_)) | (false, _) => {
						return Err(ExpressionError::NonBooleanLogic.into());
					},
				}
			},

			// no checks for these guys
			BitwiseNot | ReductionAnd | ReductionOr | ReductionXor => {},
		}

		Ok(())
	}
}

impl BinaryExpression {
	fn shallow_validate(&self, ctx: &EvalContext, _scope: &ScopeHandle) -> Result<(), EvalError> {
		let lhs_type = self.lhs.eval_type(ctx)?;
		let rhs_type = self.rhs.eval_type(ctx)?;
		let lhs_width = self.lhs.width()?.narrow_eval(ctx).ok();
		let rhs_width = self.rhs.width()?.narrow_eval(ctx).ok();

		let does_width_match = || {
			match (lhs_width, rhs_width) {
				(Some(lw), Some(rw)) => Some(lw == rw),
				(..) => None, // damn generics :((
			}
		};

		use BinaryOp::*;
		match self.op {
			// Arithmetic operators only require matching signedness
			Add | Subtract | Multiply | Divide | Modulo => {
				if lhs_type.is_signed() != rhs_type.is_signed() {
					return Err(ExpressionError::MixedSignedness.into());
				}
			},

			// Shift operators require unsigned RHS operand
			ShiftLeft | ShiftRight => {
				if rhs_type.is_signed() {
					return Err(ExpressionError::SignedShiftWidth.into());
				}
			},

			// Bitwise operators require matching operand width
			BitwiseAnd | BitwiseOr | BitwiseXor => {
				if matches!(does_width_match(), Some(false)) {
					return Err(ExpressionError::WidthMismatch.into());
				}
			},

			// Logical operators require boolean operands
			LogicalAnd | LogicalOr => {
				if lhs_type.is_signed() || rhs_type.is_signed() {
					return Err(ExpressionError::NonBooleanLogic.into());
				}

				match (lhs_width, rhs_width) {
					(Some(1), Some(1)) => {}, // okay cool
					(Some(_), Some(_)) => return Err(ExpressionError::NonBooleanLogic.into()),
					(..) => {}, // life is hard sometimes
				}
			},

			// Relational operators require matching signedness and width
			Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual | Max | Min => {
				if lhs_type.is_signed() != rhs_type.is_signed() {
					return Err(ExpressionError::MixedSignedness.into());
				}

				if matches!(does_width_match(), Some(false)) {
					return Err(ExpressionError::WidthMismatch.into());
				}
			},
		}

		Ok(())
	}
}

impl BuiltinOp {
	fn shallow_validate(&self, ctx: &EvalContext, _scope: &ScopeHandle) -> Result<(), EvalError> {
		use BuiltinOp::*;
		match self {
			// Extension width must be nonzero and generic
			ZeroExtend { expr: _, width } | SignExtend { expr: _, width } => {
				let width_type = width.eval_type(ctx)?;
				let width_val = width.narrow_eval(ctx).ok();
				match (width_type.is_generic(), width_val) {
					(false, _) => return Err(ExpressionError::InvalidExtensionWidth.into()),
					(_, Some(w)) if w < 1 => return Err(ExpressionError::InvalidExtensionWidth.into()),
					(..) => {},
				}
			},

			BusSelect { expr, msb, lsb } => {
				let lsb_type = lsb.eval_type(ctx)?;
				let msb_type = msb.eval_type(ctx)?;

				if !lsb_type.is_generic() {
					return Err(ExpressionError::InvalidBitIndex.into());
				}

				if !msb_type.is_generic() {
					return Err(ExpressionError::InvalidBitIndex.into());
				}

				let lsb_value = lsb.narrow_eval(ctx).ok();
				let msb_value = msb.narrow_eval(ctx).ok();
				let width = expr.width()?.narrow_eval(ctx).ok();

				let index_err = match (width, lsb_value, msb_value) {
					(Some(w), Some(lsb), _) if lsb >= w => true,
					(Some(w), _, Some(msb)) if msb >= w => true,
					(_, Some(lsb), _) if lsb < 0 => true,
					(_, _, Some(msb)) if msb < 0 => true,
					(_, Some(lsb), Some(msb)) if msb < lsb => true,
					(..) => false,
				};

				if index_err {
					return Err(ExpressionError::InvalidBitIndex.into());
				}
			},

			BitSelect { expr, index } => {
				let index_type = index.eval_type(ctx)?;

				if !index_type.is_generic() {
					return Err(ExpressionError::InvalidBitIndex.into());
				}

				let index_value = index.narrow_eval(ctx).ok();
				let width = expr.width()?.narrow_eval(ctx).ok();

				let index_err = match (width, index_value) {
					(_, Some(i)) if i < 0 => true,
					(Some(w), Some(i)) if i >= w => true,
					(..) => false,
				};

				if index_err {
					return Err(ExpressionError::InvalidBitIndex.into());
				}
			},

			// Count must be generic and positive
			Replicate { expr: _, count } => {
				let count_type = count.eval_type(ctx)?;
				let count_val = count.narrow_eval(ctx).ok();
				match (count_type.is_generic(), count_val) {
					(false, _) => return Err(ExpressionError::InvalidReplicationCount.into()),
					(_, Some(n)) if n < 1 => return Err(ExpressionError::InvalidReplicationCount.into()),
					(..) => {},
				}
			},

			// Join list must not be empty
			Join(exprs) => {
				if exprs.is_empty() {
					return Err(ExpressionError::EmptyJoinList.into());
				}
			},

			Width(_) => {}, // no action needed
		}

		Ok(())
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
	fn shallow_validate(&self, ctx: &EvalContext, _scope: &ScopeHandle) -> Result<(), EvalError> {
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

			let cond_width = b.condition().width()?.narrow_eval(ctx);

			use SignalSignedness::*;
			match (cond_type.signedness, cond_width) {
				(Signed, _) => return Err(ExpressionError::NonBooleanCondition.into()),
				(Unsigned, Ok(1)) => {},
				(Unsigned, Err(_)) => {}, // could not evaluate width but whatcha gonna do (it's probably generic)
				(_, Ok(_)) => return Err(ExpressionError::NonBooleanCondition.into()),
			}
		}

		Ok(())
	}
}

fn shallow_validate_slice(slice: &SignalSlice, ctx: &EvalContext, scope: &ScopeHandle) -> Result<(), EvalError> {
	let design_handle = scope.design();
	let signal = design_handle.get_signal(slice.signal).expect("Signal not in design");

	// Validate rank
	if signal.rank() != slice.indices.len() {
		return Err(ExpressionError::SliceRankMismatch.into());
	}

	// Indices must be generic
	for index in &slice.indices {
		let index_type = index.eval_type(ctx)?;
		if !index_type.is_generic() {
			return Err(ExpressionError::VariableSliceIndex.into());
		}
	}

	// TODO we could potentially check for static out-of-bounds access here

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
		})
		.unwrap();
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
				return Err(ExpressionError::ScopeError.into());
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
		self.validate(&EvalContext::without_assumptions(scope.design()), scope)
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::design::{DesignError, DesignHandle};

	#[test]
	fn basic_scope_rule_test() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let m1 = d.new_module("m1")?;
		let m2 = d.new_module("m2")?;

		let sig_foo = m1.scope().new_signal("foo")?.generic().unsigned(16u32.into()).build()?;

		let sig_bar = m2.scope().new_signal("bar")?.generic().unsigned(16u32.into()).build()?;

		// Should fail cause bar is not accessible in m1
		let assign_result = m1.scope().assign(sig_foo.into(), sig_bar.into());
		assert!(assign_result.is_err());
		Ok(())
	}

	#[test]
	fn shadowing_test() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let m1 = d.new_module("m1")?;

		let sig_outer_foo = m1.scope().new_signal("foo")?.generic().unsigned(16u32.into()).build()?;

		let mut sc_inner = m1.scope().new_subscope()?;
		let sig_inner_foo = sc_inner.new_signal("foo")?.generic().unsigned(16u32.into()).build()?;

		// Cannot access inner foo from the outer scope
		assert!(matches!(
			m1.scope().assign(sig_inner_foo.into(), 32u32.into()),
			Err(DesignError::EvalError(EvalError::InvalidExpression(
				ExpressionError::ScopeError
			)))
		));

		// Cannot access outer foo from the inner scope due to shadowing
		assert!(matches!(
			sc_inner.assign(sig_outer_foo.into(), 32u32.into()),
			Err(DesignError::EvalError(EvalError::InvalidExpression(
				ExpressionError::ScopeError
			)))
		));

		// Can access inner foo in inner scope
		assert!(matches!(sc_inner.assign(sig_inner_foo.into(), 32u32.into()), Ok(_)));

		// Can access outer foo in inner scope
		assert!(matches!(m1.scope().assign(sig_outer_foo.into(), 32u32.into()), Ok(_)));

		Ok(())
	}

	#[test]
	fn static_bad_index_check() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let m1 = d.new_module("m1")?;

		let sig_foo = m1
			.scope()
			.new_signal("foo")?
			.constant()
			.unsigned(16u32.into())
			.build()?;

		let sig_wire = m1.scope().new_signal("w")?.constant().wire().build()?;

		m1.scope()
			.assign(sig_wire.into(), Expression::from(sig_foo).bit_select(0.into()))?;

		m1.scope()
			.assign(sig_wire.into(), Expression::from(sig_foo).bit_select(1.into()))?;

		m1.scope()
			.assign(sig_wire.into(), Expression::from(sig_foo).bit_select(15.into()))?;

		let err = m1
			.scope()
			.assign(sig_wire.into(), Expression::from(sig_foo).bit_select(16.into()));

		assert!(matches!(
			err,
			Err(DesignError::EvalError(EvalError::InvalidExpression(
				ExpressionError::InvalidBitIndex
			)))
		));
		Ok(())
	}
}
