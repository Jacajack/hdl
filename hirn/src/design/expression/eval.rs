use super::{NumericConstant, SignalId, SignalSensitivity, ExpressionError};
use crate::design::{DesignHandle, SignalSignedness, HasSignedness, signal::HasSensitivity};
use std::collections::HashMap;
use thiserror::Error;

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

	pub fn design(&self) -> Option<DesignHandle> {
		self.design.clone()
	}

	fn check_assumption(&self, signal: SignalId, indices: &Vec<i64>, value: &NumericConstant) -> Result<(), EvalError> {
		if let Some(design) = &self.design {
			let design = design.borrow();
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

	pub fn signal(&self, signal: SignalId, indices: &Vec<i64>) -> Option<&NumericConstant> {
		self.assumptions.get(&(signal, indices.to_vec())) // FIXME
	}

	pub fn scalar_signal(&self, signal: SignalId) -> Option<&NumericConstant> {
		self.signal(signal, &vec![])
	}
}

/// A trait for evaluating signedness and sensitivity level of expressions
pub trait EvaluatesType {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError>;

	fn const_eval_type(&self) -> Result<EvalType, EvalError> {
		self.eval_type(&EvalContext::default())
	}
}

pub trait Evaluates {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError>;

	fn const_eval(&self) -> Result<NumericConstant, EvalError> {
		self.eval(&EvalContext::default())
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

// TODO shrink this type
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

	#[error("Incompatible type assignment/binding")]
	IncompatibleType(EvalType, EvalType),

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