use super::{DesignHandle, SignalSensitivity, SignalId, NumericConstant, SignalSignedness};
use thiserror::Error;

#[derive(Clone, Default)]
pub struct EvalContext {
	design: Option<DesignHandle>,
	assumptions: Vec<(SignalId, NumericConstant)>,
}

impl EvalContext {
	pub fn without_assumptions(design: DesignHandle) -> Self {
		EvalContext {
			design: Some(design),
			assumptions: Vec::new(),
		}
	}

	pub fn design(&self) -> Option<DesignHandle> {
		self.design.clone()
	}

	pub fn assumptions(&self) -> &[(SignalId, NumericConstant)] {
		&self.assumptions
	}
}

pub trait EvaluatesType {
	fn eval_type(&self, ctx: &EvalContext) -> Result<EvalType, EvalError>;

	fn const_eval_type(&self) -> Result<EvalType, EvalError> {
		self.eval_type(&EvalContext::default())
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

	#[error("Cannot combine signals with such sensitivities")]
	IncompatibleSensitivity,

	#[error("Could not get signal info from design (no design in eval context)")]
	NoDesign,

	#[error("Incompatible type assignment/binding")]
	IncompatibleType(EvalType, EvalType)
}

pub struct EvalDims {
	pub width: u64,
	pub dimensions: Vec<u64>,
}

impl EvalDims {
	pub fn is_scalar(&self) -> bool {
		self.dimensions.is_empty()
	}
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

macro_rules !impl_binary_type_eval_rule {
	($trait_name: ident, $trait_func: ident, $lambda: expr) => {
		impl std::ops::$trait_name for EvalResult<EvalType> {
			type Output = Self;

			fn $trait_func(self, rhs: EvalResult<EvalType>) -> Self::Output {
				let func = $lambda;
				EvalResult::propagate(self, rhs, |lhs, rhs|{
					let result: Result<EvalType, EvalError> = func(lhs, rhs);
					result.into()
				})
			}
		}

		impl std::ops::$trait_name for EvalType {
			type Output = EvalResult<EvalType>;

			fn $trait_func(self, rhs: EvalType) -> Self::Output {
				EvalResult::Ok(self).$trait_func(EvalResult::Ok(rhs))
			}
		}
	}
}

macro_rules! impl_binary_dims_eval_rule {
	($trait_name: ident, $trait_func: ident, $lambda: expr) => {
		impl std::ops::$trait_name for EvalResult<EvalDims> {
			type Output = Self;

			fn $trait_func(self, rhs: EvalResult<EvalDims>) -> Self::Output {
				let func = $lambda;
				EvalResult::propagate(self, rhs, |lhs, rhs|{
					let result: Result<EvalDims, EvalError> = func(lhs, rhs);
					result.into()
				})
			}
		}

		impl std::ops::$trait_name for EvalDims {
			type Output = EvalResult<EvalDims>;

			fn $trait_func(self, rhs: EvalDims) -> Self::Output {
				EvalResult::Ok(self).$trait_func(EvalResult::Ok(rhs))
			}
		}
	}
}

impl_binary_dims_eval_rule!(Add, add, |lhs: EvalDims, rhs: EvalDims| {
	if !lhs.is_scalar() || !rhs.is_scalar() {
		return Err(EvalError::NonScalar);
	}
	
	Ok(EvalDims{
		width: lhs.width.max(rhs.width) + 1,
		dimensions: Vec::new(),
	})
});

impl_binary_type_eval_rule!(Add, add, |lhs: EvalType, rhs: EvalType| {
	if lhs.signedness != rhs.signedness {
		return Err(EvalError::MixedSignedness);
	}

	Ok(EvalType {
		signedness: lhs.signedness,
		sensitivity: lhs.sensitivity.combine(&rhs.sensitivity).ok_or(EvalError::IncompatibleSensitivity)?,
	})
});

pub enum EvalResult<T> {
	Ok(T),
	Err(EvalError),
}

impl<T> EvalResult<T> {
	pub fn result(self) -> Result<T, EvalError> {
		match self {
			EvalResult::Ok(value) => Ok(value),
			EvalResult::Err(err) => Err(err),
		}
	}

	pub fn propagate<F: Fn(T, T) -> Self>(lhs: EvalResult<T>, rhs: EvalResult<T>, f: F) -> EvalResult<T> {
		match (lhs, rhs) {
			(EvalResult::Ok(lhs), EvalResult::Ok(rhs)) => f(lhs, rhs),
			(EvalResult::Err(lhs), EvalResult::Err(_rhs)) => EvalResult::Err(lhs),
			(EvalResult::Err(lhs), _) => EvalResult::Err(lhs),
			(_, EvalResult::Err(rhs)) => EvalResult::Err(rhs),
		}
	}
}

impl<T> From<Result<T, EvalError>> for EvalResult<T> {
	fn from(result: Result<T, EvalError>) -> Self {
		match result {
			Ok(value) => EvalResult::Ok(value),
			Err(err) => EvalResult::Err(err),
		}
	}
}

impl<T> From<EvalResult<T>> for Result<T, EvalError> {
	fn from(result: EvalResult<T>) -> Self {
		result.result()
	}
}
