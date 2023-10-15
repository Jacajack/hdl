use crate::design::{DesignHandle, SignalSignedness};
use super::{NumericConstant, SignalId, SignalSensitivity, Expression};
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

	pub fn assume(&mut self, signal: SignalId, indices: Vec<i64>, value: NumericConstant) {
		// FIXME check if constant width matches the assumed signal
		self.assumptions.insert((signal, indices), value);
	}

	pub fn assume_scalar(&mut self, signal: SignalId, value: NumericConstant) {
		self.assume(signal, vec![], value);
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

pub struct EvalDims {
	pub width: Expression,
	pub dimensions: Vec<Expression>,
}

impl EvalDims {
	pub fn new_scalar(width: Expression) -> Self {
		EvalDims {
			width,
			dimensions: Vec::new(),
		}
	}

	pub fn is_scalar(&self) -> bool {
		self.dimensions.is_empty()
	}
}

pub trait EvaluatesDimensions {
	fn eval_dims(&self, ctx: &EvalContext) -> Result<EvalDims, EvalError>;

	fn const_eval_dims(&self) -> Result<EvalDims, EvalError> {
		self.eval_dims(&EvalContext::default())
	}
}

pub trait Evaluates {
	fn eval(&self, ctx: &EvalContext) -> Result<NumericConstant, EvalError>;

	fn const_eval(&self) -> Result<NumericConstant, EvalError> {
		self.eval(&EvalContext::default())
	}
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
