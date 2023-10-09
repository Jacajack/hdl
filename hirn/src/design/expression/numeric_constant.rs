use super::{NarrowEval, WidthExpression};
use crate::{design::{DesignError, SignalSignedness}, Expression};

use num_bigint::BigInt;

use super::EvalError;

/// Represents a numeric constant value
#[derive(Clone, Debug)]
pub struct NumericConstant {
	error: Option<EvalError>,
	value: BigInt,
	signedness: SignalSignedness,
	width: u64,
}

impl NumericConstant {
	pub fn new_invalid(error: EvalError) -> Self {
		Self{
			error: Some(error),
			value: 0.into(),
			signedness: SignalSignedness::Unsigned,
			width: 0,
		}
	}

	/// New signed constant with bit width optimal to store the provided value
	pub fn new_signed(value: BigInt) -> Self {
		let width = value.bits() + 1;
		Self::new(value, SignalSignedness::Signed, width).unwrap()
	}

	/// New unsigned constant with bit width optimal to store the provided value
	pub fn new_unsigned(value: BigInt) -> Self {
		let width = value.bits().max(1);
		Self::new(value, SignalSignedness::Unsigned, width).unwrap()
	}

	pub fn new(value: BigInt, signedness: SignalSignedness, width: u64) -> Result<Self, EvalError> {
		let min_width = value.bits();
		let sign_bit = if signedness == SignalSignedness::Signed { 1 } else { 0 };

		// Note: BigInt::bits() can evaluate to 0 for 0
		if width < (min_width + sign_bit).max(1) {
			return Err(EvalError::NumericConstantWidthTooSmall);
		}
		
		// Clamp value to specified width
		let bit_mask = (BigInt::from(1) << width) - 1;
		let masked_value = value & bit_mask;

		Ok(Self{
			error: None,
			value: masked_value,
			signedness,
			width,
		})
	}

	pub fn zero() -> NumericConstant {
		Self::new_unsigned(0.into())
	}

	pub fn one() -> NumericConstant {
		Self::new_unsigned(1.into())
	}

	pub fn signedness(&self) -> Result<SignalSignedness, EvalError> {
		match &self.error {
			Some(e) => Err(e.clone()),
			None => Ok(self.signedness),
		}
	}

	pub fn width(&self) -> Result<u64, EvalError> {
		match &self.error {
			Some(e) => Err(e.clone()),
			None => Ok(self.width),
		}
	}

	pub fn value(&self) -> Result<&BigInt, EvalError> {
		match &self.error {
			Some(e) => Err(e.clone()),
			None => Ok(&self.value),
		}
	}

	pub fn try_into_u64(&self) -> Result<u64, EvalError> {
		u64::try_from(self.value()?).or(Err(EvalError::NarrowEvalRange))
	}

	pub fn try_into_i64(&self) -> Result<i64, EvalError> {
		i64::try_from(self.value()?).or(Err(EvalError::NarrowEvalRange))
	}

	pub fn is_valid(&self) -> bool {
		self.error.is_none()
	}

	pub fn get_error(&self) -> Option<EvalError> {
		self.error.clone()
	}
}

impl From<u64> for NumericConstant {
	fn from(value: u64) -> Self {
		Self::new_unsigned(value.into())
	}
}

impl From<i64> for NumericConstant {
	fn from(value: i64) -> Self {
		Self::new_signed(value.into())
	}
}

impl From<u32> for NumericConstant {
	fn from(value: u32) -> Self {
		Self::new_unsigned(value.into())
	}
}

impl From<i32> for NumericConstant {
	fn from(value: i32) -> Self {
		Self::new_signed(value.into())
	}
}

macro_rules! impl_binary_constant_op {
	($trait_name: ident, $trait_func: ident, $lambda: expr) => {
		impl std::ops::$trait_name for NumericConstant {
			type Output = NumericConstant;

			fn $trait_func(self, other: Self) -> Self::Output {
				let func = $lambda;

				// Propagate invalid constants
				match (self.get_error(), other.get_error()) {
					(Some(lhs_err), _) => return Self::new_invalid(lhs_err),
					(_, Some(rhs_err)) => return Self::new_invalid(rhs_err),
					(None, None) => {}
				}

				// This lambda can assume that both constants are valid
				assert!(self.is_valid());
				assert!(other.is_valid());
				match func(&self, &other) {
					Ok(result) => result,
					Err(err) => Self::new_invalid(err),
				}
			}
		}
	};
}

fn check_sign_match(lhs: &NumericConstant, rhs: &NumericConstant) -> Result<(), EvalError> {
	if lhs.signedness()? != rhs.signedness()? {
		return Err(EvalError::MixedSignedness);
	}
	Ok(())
}

impl_binary_constant_op!(Add, add, |lhs: &NumericConstant, rhs: &NumericConstant| -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	let value = lhs.value()? + rhs.value()?;
	let width_expr = Expression::from(lhs.clone()) + rhs.clone().into();
	let width = width_expr.width()?.const_narrow_eval()? as u64;
	Ok(NumericConstant::new(value, lhs.signedness()?, width)?)
});

#[cfg(test)]
mod test {
	use super::*;

	fn check_value(nc: &NumericConstant, val: i64, width: u64) {
		assert_eq!(*nc.value().unwrap(), val.into());
		assert_eq!(nc.width().unwrap(), width.into());
	}

	#[test]
	fn test_add() {
		let a = NumericConstant::new_signed(5.into());
		let b = NumericConstant::new_signed(3.into());
		let c = a + b;
		check_value(&c, 8, 5);
	}
}
