use super::{Expression, DesignHandle, SignalSensitivity, SignalId, NumericConstant, SignalSignedness};
use thiserror::Error;

pub struct EvalContext {
	design: Option<DesignHandle>,
	assumptions: Vec<(SignalId, NumericConstant)>,
}

impl EvalContext {
	pub fn default() -> Self {
		Self {
			design: None,
			assumptions: Vec::new(),
		}
	}
}

pub trait EvaluatesType {
	fn eval_type(&self, ctx: &EvalContext) -> EvalResult<EvalType>;

	fn const_eval_type(&self) -> EvalResult<EvalType> {
		self.eval_type(&EvalContext::default())
	}
}

pub trait Evaluates {
	fn eval(&self, ctx: &EvalContext) -> EvalResult<NumericConstant>;

	fn const_eval(&self) -> EvalResult<NumericConstant> {
		self.eval(&EvalContext::default())
	}
}

#[derive(Debug, Clone, Error)]
pub enum EvalError {
	#[error("Value of signal was not assumed, cannot evaluate")]
	MissingAssumption(SignalId),
}

/// Provides type evaluation rules for both expressions and compile-time evaluation
pub struct EvalType {
	pub width: u64,
	pub signedness: SignalSignedness,
	pub sensitivity: SignalSensitivity,
}

macro_rules !impl_binary_type_eval_rule {
	($trait_name: ident, $trait_func: ident, $lambda: expr) => {
		impl std::ops::$trait_name for EvalResult<EvalType> {
			type Output = Self;

			fn $trait_func(self, rhs: EvalResult<EvalType>) -> Self::Output {
				use EvalResult::*;
				match (self, rhs) {
					(Ok(lhs), Ok(rhs)) => $lambda(lhs, rhs),
					(Err(lhs), Err(_rhs)) => Err(lhs),
					(Err(lhs), _) => Err(lhs),
					(_, Err(rhs)) => Err(rhs),
				}
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

impl_binary_type_eval_rule!(Add, add, |lhs: EvalType, rhs: EvalType| {
	Ok(EvalType {
		width: (lhs.width + rhs.width).max(1),
		signedness: SignalSignedness::Signed, // FIXME
		sensitivity: SignalSensitivity::Async, // FIXME
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
