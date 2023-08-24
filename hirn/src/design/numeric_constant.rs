use num_bigint::BigInt;
use super::{SignalSignedness, DesignError, eval::EvalResult, eval::{EvalType, EvaluatesType, EvaluatesDimensions}, SignalSensitivity, EvalContext};

/// Represents a numeric constant value
#[derive(Clone)]
pub struct NumericConstant {
	value: BigInt,
	signedness: SignalSignedness,
	width: u64,
}

impl NumericConstant {
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

	pub fn new(value: BigInt, signedness: SignalSignedness, width: u64) -> Result<Self, DesignError> {
		let min_width = value.bits();
		let sign_bit = if signedness == SignalSignedness::Signed { 1 } else { 0 };

		// Note: BigInt::bits() can evaluate to 0 for 0
		if width < (min_width + sign_bit).max(1) {
			return Err(DesignError::NumericConstantWidthTooSmall);
		}
		
		Ok(Self {
			value,
			signedness,
			width
		})
	}

	pub fn zero() -> NumericConstant {
		Self::new_unsigned(0.into())
	}

	pub fn one() -> NumericConstant {
		Self::new_unsigned(1.into())
	}

	pub fn signedness(&self) -> SignalSignedness {
		self.signedness
	}

	pub fn width(&self) -> u64 {
		self.width
	}

	pub fn try_into_u64(&self) -> Option<u64> {
		u64::try_from(&self.value).ok()
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

macro_rules! impl_binary_constant_op {
	($trait_name: ident, $trait_func: ident, $lambda: expr) => {
		impl std::ops::$trait_name for EvalResult<NumericConstant> {
			type Output = EvalResult<NumericConstant>;

			fn $trait_func(self, other: Self) -> Self::Output {
				let func = $lambda;

				EvalResult::<NumericConstant>::propagate(self, other, |lhs: NumericConstant, rhs: NumericConstant|{
					let value =  match func(&lhs.value, &rhs.value) {
						Ok(value) => value,
						Err(err) => return EvalResult::Err(err),
	
					};
	
					let lhs_type = lhs.const_eval_type().unwrap(); // FIXME
					let rhs_type = rhs.const_eval_type().unwrap(); // FIXME
					let result_type = lhs_type.$trait_func(rhs_type).result().unwrap(); // FIXME unwrap
	
					// FIXME unwraps
					let lhs_dims = lhs.const_eval_dims().unwrap();
					let rhs_dims = rhs.const_eval_dims().unwrap();
					let result_dims = lhs_dims.$trait_func(rhs_dims).result().unwrap();

					EvalResult::Ok(NumericConstant::new(
						value,
						result_type.signedness,
						result_dims.width
					).unwrap())
				})
			}
		}

		impl std::ops::$trait_name<NumericConstant> for NumericConstant {
			type Output = EvalResult<NumericConstant>;

			fn $trait_func(self, rhs: NumericConstant) -> Self::Output {
				EvalResult::Ok(self).$trait_func(EvalResult::Ok(rhs))
			}
		}
	}
}

impl_binary_constant_op!(Add, add, |lhs: &BigInt, rhs: &BigInt| {
	Ok(lhs + rhs)
});

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_add() {
		let a = NumericConstant::new_signed(5.into());
		let b = NumericConstant::new_signed(3.into());
		let c = (a + b).result().unwrap();
		assert_eq!(c.value, 8.into());
		assert_eq!(c.width, 5);
	}
}

