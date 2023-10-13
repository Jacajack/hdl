use super::{NarrowEval, WidthExpression};
use crate::{design::{DesignError, SignalSignedness}, Expression};

use num_bigint::{BigInt, BigUint};

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
	/// New invalid constant storing provided EvalError
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

	/// New boolean (unsigned 1-bit) constant
	pub fn new_bool(value: bool) -> Self {
		Self::new_unsigned(if value { 1.into() } else { 0.into() })
	}

	/// Creates a new NumericConstant from parts
	pub fn new(value: BigInt, signedness: SignalSignedness, width: u64) -> Result<Self, EvalError> {
		let min_width = value.bits();
		let sign_bit = if signedness == SignalSignedness::Signed { 1 } else { 0 };

		// Note: BigInt::bits() can evaluate to 0 for 0
		if width < (min_width + sign_bit).max(1) {
			return Err(EvalError::NumericConstantWidthTooSmall);
		}
		
		// Clamp value to specified width while preserving sign
		let masked_value: BigInt;
		{
			let (orig_sign, orig_mag) = value.into_parts();
			let bit_mask = (BigUint::from(1u32) << width) - 1u32;
			let masked_mag = orig_mag & bit_mask;
			masked_value = BigInt::from_biguint(orig_sign, masked_mag);
		}

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

	pub fn to_hex_str(&self) -> String {
		format!("{:x}", self.value)
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

	pub fn is_bool(&self) -> bool {
		self.is_valid() && self.width == 1 && self.signedness == SignalSignedness::Unsigned
	}

	pub fn is_zero(&self) -> bool {
		self.is_valid() && self.value == 0.into()
	}

	pub fn is_nonzero(&self) -> bool {
		self.is_valid() && !self.is_zero()
	}

	pub fn get_error(&self) -> Option<EvalError> {
		self.error.clone()
	}

	/// Performs less-than comparison. The result is a boolean NumericConstant
	pub fn op_lt(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_sign_match(self, rhs))
			.unwrap_or(Self::new_bool(self < rhs))
	}

	pub fn op_lte(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_sign_match(self, rhs))
			.unwrap_or(Self::new_bool(self <= rhs))
	}

	pub fn op_gt(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_sign_match(self, rhs))
			.unwrap_or(Self::new_bool(self > rhs))
	}

	pub fn op_gte(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_sign_match(self, rhs))
			.unwrap_or(Self::new_bool(self >= rhs))
	}

	pub fn op_eq(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_sign_match(self, rhs))
			.unwrap_or(Self::new_bool(self == rhs))
	}

	pub fn op_ne(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_sign_match(self, rhs))
			.unwrap_or(Self::new_bool(self != rhs))
	}

	pub fn op_min(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_sign_match(self, rhs))
			.unwrap_or(if self < rhs {self.clone()} else {rhs.clone()})
	}

	pub fn op_max(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_sign_match(self, rhs))
			.unwrap_or(if self > rhs {self.clone()} else {rhs.clone()})
	}

	pub fn op_land(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_boolean_operands(self, rhs))
			.unwrap_or(Self::new_bool(self.is_nonzero() && rhs.is_nonzero()))
	}

	pub fn op_lor(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or(Self::require_boolean_operands(self, rhs))
			.unwrap_or(Self::new_bool(self.is_nonzero() || rhs.is_nonzero()))
	}

	// FIXME as trait
	pub fn op_lnot(&self) -> Self {
		match self.is_valid() {
			false => self.clone(),
			true => Self::new_bool(self.is_nonzero()), // FIXME ensure boolean operand
		}
	}

	fn count_ones(&self) -> u64 {
		assert!(self.is_valid());
		let (_sign, value) = self.value.clone().into_parts();
		value.count_ones()
	}

	pub fn op_reduction_and(&self) -> Self {
		if !self.is_valid() {return self.clone();}
		Self::new_bool(self.count_ones() == self.width)
	}

	pub fn op_reduction_or(&self) -> Self {
		if !self.is_valid() {return self.clone();}
		Self::new_bool(self.count_ones() > 0)
	}

	pub fn op_reduction_xor(&self) -> Self {
		if !self.is_valid() {return self.clone();}
		Self::new_bool(self.count_ones() % 2 == 1)
	}


	fn propagate_err(lhs: &NumericConstant, rhs: &NumericConstant) -> Option<NumericConstant> {
		match (lhs.get_error(), rhs.get_error()) {
			(Some(lhs_err), _) => Some(Self::new_invalid(lhs_err)),
			(_, Some(rhs_err)) => Some(Self::new_invalid(rhs_err)),
			(None, None) => None,
		}
	}

	fn require_sign_match(lhs: &Self, rhs: &Self) -> Option<NumericConstant> {
		if lhs.signedness != rhs.signedness {
			return Some(Self::new_invalid(EvalError::MixedSignedness));
		}
		None
	}

	fn require_width_match(lhs: &Self, rhs: &Self) -> Option<NumericConstant> {
		if lhs.width != rhs.width {
			return Some(Self::new_invalid(EvalError::WidthMismatch));
		}
		None
	}

	fn require_boolean_operands(lhs: &Self, rhs: &Self) -> Option<NumericConstant> {
		if !lhs.is_bool() || !rhs.is_bool() {
			return Some(Self::new_invalid(EvalError::NonBooleanOperand));
		}
		None
	}

	fn normalize_unsigned(&mut self) {
		assert!(self.is_valid());
		assert!(self.signedness == SignalSignedness::Unsigned);
		if self.value.sign() == num_bigint::Sign::Minus {
			let base = BigInt::from(1u32) << self.width;
			let (_sign, mag) = (base + self.value.clone()).into_parts();
			self.value = BigInt::from_biguint(num_bigint::Sign::Plus, mag);
		}
	}
}

impl From<bool> for NumericConstant {
	fn from(value: bool) -> Self {
		Self::new_bool(value)
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

fn check_sign_match(lhs: &NumericConstant, rhs: &NumericConstant) -> Result<(), EvalError> {
	if lhs.signedness()? != rhs.signedness()? {
		return Err(EvalError::MixedSignedness);
	}
	Ok(())
}

fn check_width_match(lhs: &NumericConstant, rhs: &NumericConstant) -> Result<(), EvalError> {
	if lhs.width()? != rhs.width()? {
		return Err(EvalError::WidthMismatch);
	}
	Ok(())
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

macro_rules! impl_rust_binary_constant_op {
	($trait_name: ident, $trait_func: ident, $operator: tt) => {
		impl_binary_constant_op!($trait_name, $trait_func, |lhs: &NumericConstant, rhs: &NumericConstant| -> Result<NumericConstant, EvalError> {
			check_sign_match(lhs, rhs)?;
			let width_expr = Expression::from(lhs.clone()) $operator rhs.clone().into();
			let mut result = NumericConstant::new(
				lhs.value()? $operator rhs.value()?,
				lhs.signedness()?,
				width_expr.width()?.const_narrow_eval()? as u64
			)?;
			if lhs.signedness()? == SignalSignedness::Unsigned {
				result.normalize_unsigned();
			}
			Ok(result)
		});
	}
}

impl PartialEq for NumericConstant {
	fn eq(&self, other: &Self) -> bool {
		match (self.is_valid(), other.is_valid()) {
			(false, false) => true,
			(false, true) => false,
			(true, false) => false,
			(true, true) => self.value == other.value,
		}
	}
}

impl Ord for NumericConstant {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		use std::cmp::Ordering::*;
		match (self.is_valid(), other.is_valid()) {
			(false, false) => Equal,
			(false, true) => Greater,
			(true, false) => Less,
			(true, true) => self.value.cmp(&other.value),
		}
	}
}

impl PartialOrd for NumericConstant {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Eq for NumericConstant {}

impl_rust_binary_constant_op!(Add, add, +);
impl_rust_binary_constant_op!(Sub, sub, -);
impl_rust_binary_constant_op!(Mul, mul, *);
impl_rust_binary_constant_op!(Div, div, /);
impl_rust_binary_constant_op!(Rem, rem, %);

impl_binary_constant_op!(Shl, shl,  |lhs: &NumericConstant, rhs: &NumericConstant| -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	let width_expr = Expression::from(lhs.clone()) << rhs.clone().into();
	let mut result = NumericConstant::new(
		lhs.value()? << rhs.try_into_u64()?, // FIXME force reasonable RHS range
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64
	)?;
	if lhs.signedness()? == SignalSignedness::Unsigned {
		result.normalize_unsigned();
	}
	Ok(result)
});

impl_binary_constant_op!(Shr, shr,  |lhs: &NumericConstant, rhs: &NumericConstant| -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	let width_expr = Expression::from(lhs.clone()) >> rhs.clone().into();
	let mut result = NumericConstant::new(
		lhs.value()? >> rhs.try_into_u64()?, // FIXME force reasonable RHS range
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64
	)?;
	if lhs.signedness()? == SignalSignedness::Unsigned {
		result.normalize_unsigned();
	}
	Ok(result)
});

impl_binary_constant_op!(BitAnd, bitand,  |lhs: &NumericConstant, rhs: &NumericConstant| -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let width_expr = Expression::from(lhs.clone()) & rhs.clone().into();
	let mut result = NumericConstant::new(
		lhs.value()? & rhs.value()?,
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64
	)?;
	if lhs.signedness()? == SignalSignedness::Unsigned {
		result.normalize_unsigned();
	}
	Ok(result)
});

impl_binary_constant_op!(BitOr, bitor,  |lhs: &NumericConstant, rhs: &NumericConstant| -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let width_expr = Expression::from(lhs.clone()) | rhs.clone().into();
	let mut result = NumericConstant::new(
		lhs.value()? | rhs.value()?,
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64
	)?;
	if lhs.signedness()? == SignalSignedness::Unsigned {
		result.normalize_unsigned();
	}
	Ok(result)
});

impl_binary_constant_op!(BitXor, bitxor,  |lhs: &NumericConstant, rhs: &NumericConstant| -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let width_expr = Expression::from(lhs.clone()) ^ rhs.clone().into();
	let mut result = NumericConstant::new(
		lhs.value()? ^ rhs.value()?,
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64
	)?;
	if lhs.signedness()? == SignalSignedness::Unsigned {
		result.normalize_unsigned();
	}
	Ok(result)
});


#[cfg(test)]
mod test {
	use super::*;

	fn check_value(nc: NumericConstant, val: i64, width: u64) {
		assert_eq!(*nc.value().unwrap(), BigInt::from(val));
		assert_eq!(nc.width().unwrap(), width.into());
	}

	fn check_value_u(nc: NumericConstant, val: u64, width: u64) {
		assert_eq!(*nc.value().unwrap(), BigInt::from(val));
		assert_eq!(nc.width().unwrap(), width.into());
	}

	fn nc(v: i64) -> NumericConstant {
		NumericConstant::new_signed(v.into())
	}

	fn ncu(v: u64) -> NumericConstant {
		NumericConstant::new_unsigned(v.into())
	}

	#[test]
	fn test_add() {
		check_value(nc(0) + nc(0), 0, 2);
		check_value(nc(5) + nc(3), 8, 5);
		check_value_u(ncu(0) + ncu(0), 0, 2);
		check_value_u(ncu(1024) + ncu(1), 1025, 12);
		check_value_u(ncu(1024) + ncu(1024), 2048, 12);
	}

	#[test]
	fn test_sub() {
		check_value(nc(4) - nc(7), -3, 5);
		check_value(nc(10) - nc(9), 1, 6);
		check_value(nc(1) - nc(1), 0, 3);
		check_value_u(ncu(1024) - ncu(1024), 0, 12);
		check_value_u(ncu(0) - ncu(1), 3, 2);
		check_value_u(ncu(127) - ncu(128), 511, 9);
	}

	#[test]
	fn test_mul() {
		check_value(nc(0) * nc(0), 0, 2);
		check_value(nc(17) * nc(-1), -17, 8); // 6b * 2b => 8b
		check_value(nc(-1) * nc(-1), 1, 4); 
		check_value(nc(10) * nc(10), 100, 10);
		check_value(ncu(10) * ncu(10), 100, 8);
	}

	#[test]
	fn test_div() {
		check_value(nc(30) / nc(3), 10, 6);
		check_value(nc(31) / nc(-1), -31, 6);
		check_value(nc(-31) / nc(-1), 31, 6);
		check_value_u(ncu(255) / ncu(2), 127, 8);
		check_value_u(ncu(255) / ncu(1), 255, 8);
		check_value_u(ncu(30) / ncu(3), 10, 5);
	}

	#[test]
	fn test_rem() {
		check_value(nc(-5) % nc(3), -5 % 3, 3);
		check_value(nc(-5) % nc(-3), -5 % -3, 3);
		check_value(nc(1247) % nc(1), 0, 2);
		check_value(nc(1247) % nc(1247), 0, 12);
		check_value_u(ncu(5) % ncu(3), 2, 2);
		check_value_u(ncu(560) % ncu(33), 560 % 33, 6);
	}

	#[test]
	fn test_bitwise_and() {
		check_value_u(ncu(0b1010) & ncu(0b1100), 0b1000, 4);
		// TODO tests for width mismatch
	}

	#[test]
	fn test_bitwise_or() {
		check_value_u(ncu(0b1010) | ncu(0b1100), 0b1110, 4);
		// TODO tests for width mismatch
	}

	#[test]
	fn test_bitwise_xor() {
		check_value_u(ncu(0b1010) ^ ncu(0b1100), 0b0110, 4);
		// TODO tests for width mismatch
	}

	#[test]
	fn test_logic() {
		let t = NumericConstant::new_bool(true);
		let f = NumericConstant::new_bool(false);

		assert_eq!(t.op_land(&t), t);
		assert_eq!(t.op_land(&f), f);
		assert_eq!(f.op_land(&t), f);
		assert_eq!(f.op_land(&f), f);

		assert_eq!(t.op_lor(&t), t);
		assert_eq!(t.op_lor(&f), t);
		assert_eq!(f.op_lor(&t), t);
		assert_eq!(f.op_lor(&f), f);
	}

}
