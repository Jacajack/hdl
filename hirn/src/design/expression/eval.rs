use super::{ExpressionError, NumericConstant, SignalId, SignalSensitivity};
use crate::design::{signal::HasSensitivity, DesignHandle, HasSignedness, SignalSignedness};
use std::collections::HashMap;
use thiserror::Error;

pub trait EvalAssumptions {
	fn design(&self) -> Option<DesignHandle>;

	fn signal(&self, signal: SignalId, indices: &Vec<i64>) -> Option<&NumericConstant>;

	fn scalar_signal(&self, signal: SignalId) -> Option<&NumericConstant> {
		self.signal(signal, &vec![])
	}
}

#[derive(Clone, Default)]
pub struct EvalContext {
	design: Option<DesignHandle>,
	assumptions: HashMap<(SignalId, Vec<i64>), NumericConstant>,
}

impl EvalContext {
	pub fn without_assumptions(design: DesignHandle) -> Self {
		EvalContext {
			design: Some(design),
			assumptions: HashMap::new(),
		}
	}

	fn check_assumption(&self, signal: SignalId, indices: &Vec<i64>, value: &NumericConstant) -> Result<(), EvalError> {
		if let Some(design) = &self.design {
			let sig = design.get_signal(signal).unwrap();

			// We cannot fully check for errors here - e.g. in case where
			// an array has generic dimensions/width

			// TODO more checks here

			if sig.rank() != indices.len() {
				return Err(AssumptionError::RankMismatch.into());
			}

			if sig.class.signedness() != value.signedness()? {
				return Err(AssumptionError::SignednessMismatch.into());
			}
		}

		Ok(())
	}

	pub fn assume_array(
		&mut self,
		signal: SignalId,
		indices: Vec<i64>,
		value: NumericConstant,
	) -> Result<(), EvalError> {
		self.check_assumption(signal, &indices, &value)?;
		self.assumptions.insert((signal, indices), value);
		Ok(())
	}

	pub fn assume(&mut self, signal: SignalId, value: NumericConstant) -> Result<(), EvalError> {
		self.assume_array(signal, vec![], value)
	}

	pub fn scalar_signal(&self, signal: SignalId) -> Option<&NumericConstant> {
		self.signal(signal, &vec![])
	}
}

impl EvalAssumptions for EvalContext {
	fn design(&self) -> Option<DesignHandle> {
		self.design.clone()
	}

	fn signal(&self, signal: SignalId, indices: &Vec<i64>) -> Option<&NumericConstant> {
		self.assumptions.get(&(signal, indices.to_vec())) // FIXME
	}
}

/// A trait for evaluating signedness and sensitivity level of expressions
pub trait EvaluatesType {
	fn eval_type(&self, ctx: &dyn EvalAssumptions) -> Result<EvalType, EvalError>;
}

pub trait Evaluates {
	fn eval(&self, ctx: &dyn EvalAssumptions) -> Result<NumericConstant, EvalError>;

	fn eval_to<T: TryFrom<NumericConstant, Error = EvalError>>(
		&self,
		ctx: &dyn EvalAssumptions,
	) -> Result<T, EvalError> {
		self.eval(ctx)?.try_into()
	}

	fn try_eval_ignore_missing(&self, ctx: &dyn EvalAssumptions) -> Result<Option<NumericConstant>, EvalError> {
		match self.eval(ctx) {
			Ok(value) => Ok(Some(value)),
			Err(EvalError::MissingAssumption(_)) => Ok(None),
			Err(err) => Err(err),
		}
	}

	fn try_eval_ignore_missing_into<T: TryFrom<NumericConstant, Error = EvalError>>(
		&self,
		ctx: &dyn EvalAssumptions,
	) -> Result<Option<T>, EvalError> {
		Ok(match self.try_eval_ignore_missing(ctx)? {
			Some(value) => Some(value.try_into()?),
			None => None,
		})
	}
}

#[derive(Debug, Clone, Copy, Error)]
pub enum AssumptionError {
	#[error("Assumption does not refer to an array signal properly")]
	RankMismatch,

	#[error("Assumed value's bit width is different from signal's width")]
	WidthMismatch,

	#[error("Signal's and assumed value's signedness differ")]
	SignednessMismatch,
}

#[derive(Debug, Clone, Error)]
pub enum EvalError {
	#[error("Value of signal was not assumed, cannot evaluate")]
	MissingAssumption(SignalId),

	#[error("Non-scalar value used in arithmetic")]
	NonScalar,

	#[error("Mixed signedness")]
	MixedSignedness,

	#[error("Width mismatch")]
	WidthMismatch,

	#[error("Empty join list")]
	EmptyJoinList,

	#[error("Non-boolean operand in logical expression")]
	NonBooleanOperand,

	#[error("Cannot combine signals with such sensitivities")]
	IncompatibleSensitivity,

	#[error("Could not get signal info from design (no design in eval context)")]
	NoDesign,

	#[error("Invalid array index")]
	InvalidIndex,

	#[error("Invalid index rank")]
	InvalidIndexRank,

	#[error("Narrow type evaluation not supported")]
	NarrowEvalNotSupported,

	#[error("Result outside of narrow eval range")]
	NarrowEvalRange,

	#[error("Invalid constant")]
	InvalidConstant,

	#[error("Invalid sensitivity combination")]
	InvalidSensitivityCombination,

	#[error("Numeric constant width too small")]
	NumericConstantWidthTooSmall,

	#[error("Shift width too large or negative")]
	BadShiftWidth,

	#[error("Signed shift RHS")]
	SignedShiftRhs,

	#[error("ext/sext/zext width cannot be signed")]
	SignedWidth,

	#[error("Cannot use ext/sext/zext to shrink")]
	CannotShrink,

	#[error("Cannot negate unsigned")]
	NegateUnsigned,

	#[error("Invalid index in bit select")]
	BadBitSelect,

	#[error("Invalid LSB, MSB indices in bus select")]
	BadBusSelect,

	#[error("Division by zero")]
	DivisionByZero,

	#[error(transparent)]
	InvalidAssumption(#[from] AssumptionError),

	#[error(transparent)]
	InvalidExpression(#[from] ExpressionError),
}

/// Provides type evaluation rules for both expressions and compile-time evaluation
#[derive(Clone, Debug)]
pub struct EvalType {
	pub signedness: SignalSignedness,
	pub sensitivity: SignalSensitivity,
}

impl EvalType {
	pub fn can_drive(&self, other: &EvalType) -> bool {
		self.signedness == other.signedness && self.sensitivity.can_drive(&other.sensitivity)
	}

	pub fn can_drive_check_clk(&self, other: &EvalType) -> bool {
		self.signedness == other.signedness && self.sensitivity.can_drive_check_clk(&other.sensitivity)
	}
}

impl HasSignedness for EvalType {
	fn signedness(&self) -> SignalSignedness {
		self.signedness
	}
}

impl HasSensitivity for EvalType {
	fn sensitivity(&self) -> &SignalSensitivity {
		&self.sensitivity
	}
}
