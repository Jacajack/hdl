use std::cmp::max;
use crate::design::{HasSignedness, SignalSignedness};

use log::error;
use num_bigint::{BigInt, BigUint};

use super::EvalError;

/// Represents a numeric constant value
#[derive(Clone, Debug)]
pub struct NumericConstant {
	error: Option<EvalError>,
	value: BigUint,
	signedness: SignalSignedness,
	width: u64,
}

fn not_biguint(value: BigUint, width: u64) -> BigUint {
	let mask = (BigUint::from(1u32) << width) - 1u32;
	value ^ mask
}

fn neg_biguint(value: BigUint, width: u64) -> BigUint {
	not_biguint(value, width) + 1u32
}

fn signed_bigint_min_width(value: &BigInt) -> u64 {
	let (sign, mag) = value.clone().into_parts();
	max(
		1,
		mag.bits()
			+ if mag.count_ones() == 1 && sign == num_bigint::Sign::Minus {
				0
			}
			else {
				1
			},
	)
}

fn unsigned_bigint_min_width(value: &BigInt) -> u64 {
	max(1, value.bits())
}

impl NumericConstant {
	const MAX_SHIFT_WIDTH: u64 = 65535;

	/// New invalid constant storing provided EvalError
	pub fn new_invalid(error: EvalError) -> Self {
		Self {
			error: Some(error),
			value: 0u32.into(),
			signedness: SignalSignedness::Unsigned,
			width: 0,
		}
	}

	/// New signed constant with bit width optimal to store the provided value
	pub fn new_signed(value: BigInt) -> Self {
		Self::from_bigint(value.clone(), SignalSignedness::Signed, signed_bigint_min_width(&value)).unwrap()
	}

	/// New unsigned constant with bit width optimal to store the provided value
	pub fn new_unsigned(value: BigInt) -> Self {
		let width = value.bits().max(1);
		Self::from_bigint(value, SignalSignedness::Unsigned, width).unwrap()
	}

	/// New boolean (unsigned 1-bit) constant
	pub fn new_bool(value: bool) -> Self {
		Self::new_unsigned(if value { 1.into() } else { 0.into() })
	}

	pub fn from_bigint(value: BigInt, signedness: SignalSignedness, width: u64) -> Result<Self, EvalError> {
		let min_width = if signedness.is_signed() {
			signed_bigint_min_width(&value)
		}
		else {
			unsigned_bigint_min_width(&value)
		};

		if width < min_width {
			error!(
				"Invalid num. const - sign: {:?}, width: {}, value: {}, min width is {}",
				signedness, width, value, min_width
			);
			return Err(EvalError::NumericConstantWidthTooSmall);
		}

		Self::new(
			{
				let (sign, mut mag) = value.into_parts();
				if sign == num_bigint::Sign::Minus {
					mag = neg_biguint(mag, width)
				}
				mag
			},
			signedness,
			width,
		)
	}

	/// Creates a new NumericConstant from parts
	pub fn new(value: BigUint, signedness: SignalSignedness, width: u64) -> Result<Self, EvalError> {
		if value.bits() > width {
			error!(
				"Invalid num. const - sign: {:?}, width: {}, value: {}, min width is {}",
				signedness,
				width,
				value,
				value.bits()
			);
			return Err(EvalError::NumericConstantWidthTooSmall);
		}

		let mut nc = Self {
			error: None,
			value,
			signedness,
			width,
		};

		nc.normalize();
		Ok(nc)
	}

	pub fn zero() -> NumericConstant {
		Self::new_unsigned(0.into())
	}

	pub fn one() -> NumericConstant {
		Self::new_unsigned(1.into())
	}

	/// Returns the most significant bit (sign bit in case of signed numbers)
	pub fn msb(&self) -> Result<bool, EvalError> {
		match &self.error {
			Some(e) => Err(e.clone()),
			None => {
				if self.value.bits() < self.width {
					Ok(false)
				}
				else {
					Ok(self.value.bit(self.width - 1))
				}
			},
		}
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

	fn to_bigint(&self) -> Result<BigInt, EvalError> {
		if self.signedness()? == SignalSignedness::Unsigned {
			Ok(BigInt::from_biguint(num_bigint::Sign::Plus, self.value.clone()))
		}
		else {
			if self.msb()? {
				Ok(BigInt::from_biguint(
					num_bigint::Sign::Minus,
					neg_biguint(self.value.clone(), self.width),
				))
			}
			else {
				Ok(BigInt::from_biguint(num_bigint::Sign::Plus, self.value.clone()))
			}
		}
	}

	fn to_biguint(&self) -> Result<&BigUint, EvalError> {
		match &self.error {
			Some(e) => Err(e.clone()),
			None => Ok(&self.value),
		}
	}

	pub fn as_unsigned(mut self) -> Self {
		self.signedness = SignalSignedness::Unsigned;
		self
	}

	pub fn as_signed(mut self) -> Self {
		self.signedness = SignalSignedness::Signed;
		self
	}

	pub fn to_hex_str(&self) -> Result<String, EvalError> {
		match self.get_error() {
			Some(e) => Err(e.clone()),
			None => Ok(format!("{:x}", self.value)),
		}
	}

	pub fn to_dec_str(&self) -> Result<String, EvalError> {
		match self.get_error() {
			Some(e) => Err(e.clone()),
			None => Ok(format!("{}", self.value)),
		}
	}

	pub fn try_into_u64(&self) -> Result<u64, EvalError> {
		if let Some(ref err) = self.error {
			return Err(err.clone());
		}
		u64::try_from(&self.value).or(Err(EvalError::NarrowEvalRange))
	}

	pub fn try_into_i64(&self) -> Result<i64, EvalError> {
		if let Some(ref err) = self.error {
			return Err(err.clone());
		}
		i64::try_from(self.to_bigint()?).or(Err(EvalError::NarrowEvalRange))
	}

	pub fn is_valid(&self) -> bool {
		self.error.is_none()
	}

	pub fn is_bool(&self) -> bool {
		self.is_valid() && self.width == 1 && self.signedness == SignalSignedness::Unsigned
	}

	pub fn is_zero(&self) -> bool {
		self.is_valid() && self.value == 0u32.into()
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
			.or_else(|| Self::require_sign_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self < rhs))
	}

	pub fn op_lte(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self <= rhs))
	}

	pub fn op_gt(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self > rhs))
	}

	pub fn op_gte(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self >= rhs))
	}

	pub fn op_eq(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self == rhs))
	}

	pub fn op_ne(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self != rhs))
	}

	pub fn op_min(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.unwrap_or_else(|| {
				if self < rhs {
					self.clone()
				}
				else {
					rhs.clone()
				}
			})
	}

	pub fn op_max(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.unwrap_or_else(|| {
				if self > rhs {
					self.clone()
				}
				else {
					rhs.clone()
				}
			})
	}

	pub fn op_land(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_boolean_operands(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self.is_nonzero() && rhs.is_nonzero()))
	}

	pub fn op_lor(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_boolean_operands(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self.is_nonzero() || rhs.is_nonzero()))
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
		self.value.count_ones()
	}

	pub fn get_bit(&self, n: u64) -> Option<bool> {
		match &self.error {
			Some(_) => None,
			None => Some(self.value.bit(n)),
		}
	}

	pub fn op_reduction_and(&self) -> Self {
		if !self.is_valid() {
			return self.clone();
		}
		Self::new_bool(self.count_ones() == self.width)
	}

	pub fn op_reduction_or(&self) -> Self {
		if !self.is_valid() {
			return self.clone();
		}
		Self::new_bool(self.count_ones() > 0)
	}

	pub fn op_reduction_xor(&self) -> Self {
		if !self.is_valid() {
			return self.clone();
		}
		Self::new_bool(self.count_ones() % 2 == 1)
	}

	fn op_lsl_i(&self, rhs: u64, expand: bool) -> Self {
		if !self.is_valid() {
			return self.clone();
		}
		if rhs > Self::MAX_SHIFT_WIDTH {
			return Self::new_invalid(EvalError::BadShiftWidth);
		}
		let mut result = self.clone();
		result.value <<= rhs;
		if expand {
			result.width += rhs;
		}
		else {
			result.normalize();
		}
		result
	}

	fn op_lsr_i(&self, rhs: u64) -> Self {
		if !self.is_valid() {
			return self.clone();
		}
		if rhs > Self::MAX_SHIFT_WIDTH {
			return Self::new_invalid(EvalError::BadShiftWidth);
		}
		let mut result = self.clone();
		result.value >>= rhs;
		result.normalized()
	}

	fn op_asr_i(&self, rhs: u64) -> Self {
		if !self.is_valid() {
			return self.clone();
		}
		let msb = self.msb().expect("self is valid");
		let mut result = self.op_lsr_i(rhs);
		for i in 0..(rhs.min(self.width)) {
			result.value.set_bit(self.width - 1 - i, msb);
		}
		result.normalized()
	}

	fn check_shift_rhs(rhs: &NumericConstant) -> Option<Self> {
		match rhs.signedness() {
			Ok(SignalSignedness::Unsigned) => None,
			Ok(SignalSignedness::Signed) => Some(Self::new_invalid(EvalError::SignedShiftRhs)),
			Err(err) => Some(Self::new_invalid(err)),
		}
	}

	/// Arithmetic shift right
	pub fn op_asr(&self, rhs: NumericConstant) -> Self {
		if let Some(err) = Self::check_shift_rhs(&rhs) {
			return err;
		}
		match rhs.try_into_u64() {
			Ok(rhs) => self.op_asr_i(rhs),
			Err(err) => Self::new_invalid(err),
		}
	}

	/// Logical shift right
	pub fn op_lsr(&self, rhs: NumericConstant) -> Self {
		if let Some(err) = Self::check_shift_rhs(&rhs) {
			return err;
		}
		match rhs.try_into_u64() {
			Ok(rhs) => self.op_lsr_i(rhs),
			Err(err) => Self::new_invalid(err),
		}
	}

	/// Shift left
	pub fn op_shl(&self, rhs: NumericConstant) -> Self {
		if let Some(err) = Self::check_shift_rhs(&rhs) {
			return err;
		}
		match rhs.try_into_u64() {
			Ok(rhs) => self.op_lsl_i(rhs, false),
			Err(err) => Self::new_invalid(err),
		}
	}

	// Shift left with expansion
	pub fn op_shl_expand(&self, rhs: NumericConstant) -> Self {
		if let Some(err) = Self::check_shift_rhs(&rhs) {
			return err;
		}
		match rhs.try_into_u64() {
			Ok(rhs) => self.op_lsl_i(rhs, true),
			Err(err) => Self::new_invalid(err),
		}
	}

	/// Shift right (behavior depends on signedness)
	fn op_shr(&self, rhs: NumericConstant) -> Self {
		match self.signedness {
			SignalSignedness::Unsigned => self.op_lsr(rhs),
			SignalSignedness::Signed => self.op_asr(rhs),
		}
	}

	pub fn op_bit_select(&self, rhs: NumericConstant) -> Self {
		if let Some(err) = self.get_error() {
			return Self::new_invalid(err);
		}
		if rhs.signedness == SignalSignedness::Signed {
			return Self::new_invalid(EvalError::SignedWidth);
		}
		match rhs.try_into_u64() {
			Err(_) => Self::new_invalid(EvalError::BadBitSelect),
			Ok(n) => {
				if n >= self.width {
					Self::new_invalid(EvalError::BadBitSelect)
				}
				else {
					Self::new_bool(self.value.bit(n))
				}
			},
		}
	}

	fn op_bus_select_i(&self, lsb: u64, msb: u64) -> Self {
		if msb < lsb || msb >= self.width || lsb >= self.width {
			return Self::new_invalid(EvalError::BadBusSelect);
		}
		let new_width = msb - lsb + 1;
		let mask = (BigUint::from(1u32) << new_width) - 1u32;
		Self::new((self.value.clone() >> lsb) & mask, self.signedness, new_width)
			.expect("new() params must be valid here")
	}

	pub fn op_bus_select(&self, lsb: NumericConstant, msb: NumericConstant) -> Self {
		match (lsb.signedness, msb.signedness) {
			(SignalSignedness::Signed, _) | (_, SignalSignedness::Signed) => {
				return Self::new_invalid(EvalError::SignedWidth);
			},
			_ => {},
		}

		match (lsb.try_into_u64(), msb.try_into_u64()) {
			(Err(e), _) | (_, Err(e)) => Self::new_invalid(e),
			(Ok(lsb), Ok(msb)) => self.op_bus_select_i(lsb, msb),
		}
	}

	pub fn op_bitwise_not(&self) -> Self {
		if let Some(err) = self.get_error() {
			return Self::new_invalid(err);
		}
		let mut result = self.clone();
		result.value = not_biguint(result.value, self.width);
		result.normalized()
	}

	pub fn op_neg(&self) -> Self {
		if let Some(err) = self.get_error() {
			return Self::new_invalid(err);
		}
		if self.signedness == SignalSignedness::Unsigned {
			return Self::new_invalid(EvalError::NegateUnsigned);
		}

		let mut result = self.clone();
		result.value = neg_biguint(result.value, self.width);
		result.normalized()
	}

	fn op_zext_i(&self, n: u64) -> Self {
		if let Some(err) = self.get_error() {
			return Self::new_invalid(err);
		}
		if n < self.width {
			return Self::new_invalid(EvalError::CannotShrink);
		}
		let mut result = self.clone();
		result.width = n;
		result.normalized()
	}

	fn op_sext_i(&self, n: u64) -> Self {
		let mut result = self.op_zext_i(n);
		if !result.is_valid() {
			return result;
		}
		let msb = self.msb().expect("self is valid");
		for n in (self.width)..n {
			result.value.set_bit(n, msb);
		}
		result.normalized()
	}

	pub fn op_zext(&self, rhs: NumericConstant) -> Self {
		match (rhs.try_into_u64(), rhs.signedness()) {
			(_, Err(e)) | (Err(e), _) => Self::new_invalid(e),
			(_, Ok(SignalSignedness::Signed)) => Self::new_invalid(EvalError::SignedWidth),
			(Ok(n), Ok(SignalSignedness::Unsigned)) => self.op_zext_i(n),
		}
	}

	pub fn op_sext(&self, rhs: NumericConstant) -> Self {
		match (rhs.try_into_u64(), rhs.signedness()) {
			(_, Err(e)) | (Err(e), _) => Self::new_invalid(e),
			(_, Ok(SignalSignedness::Signed)) => Self::new_invalid(EvalError::SignedWidth),
			(Ok(n), Ok(SignalSignedness::Unsigned)) => self.op_sext_i(n),
		}
	}

	pub fn op_ext(&self, rhs: NumericConstant) -> Self {
		match self.signedness {
			SignalSignedness::Unsigned => self.op_zext(rhs),
			SignalSignedness::Signed => self.op_sext(rhs),
		}
	}

	pub fn join(values: Vec<NumericConstant>) -> Self {
		if values.is_empty() {
			return Self::new_invalid(EvalError::EmptyJoinList);
		}

		// Propagate errors from any argument
		for v in &values {
			if let Some(err) = v.get_error() {
				return Self::new_invalid(err);
			}
		}

		let result_width: u64 = values.iter().map(|v| v.width).sum();
		assert_ne!(result_width, 0, "result width is zero");
		let mut result = Self::new(0u32.into(), SignalSignedness::Unsigned, result_width)
			.expect("Self::new() has no reason to fail here");

		let mut result_bit_num = result_width;
		for arg in &values {
			for arg_bit_num in (0..arg.width).rev() {
				result_bit_num -= 1;
				result.value.set_bit(result_bit_num, arg.value.bit(arg_bit_num));
			}
		}

		assert_eq!(result_bit_num, 0);

		result
	}

	pub fn op_join(self, rhs: NumericConstant) -> Self {
		Self::join(vec![self, rhs])
	}

	pub fn op_replicate(self, rhs: NumericConstant) -> Self {
		if let Some(err) = rhs.get_error() {
			return Self::new_invalid(err);
		}
		match rhs.try_into_u64() {
			Err(err) => Self::new_invalid(err),
			Ok(n) => Self::join(vec![self; n as usize]),
		}
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

	fn require_boolean_operands(lhs: &Self, rhs: &Self) -> Option<NumericConstant> {
		if !lhs.is_bool() || !rhs.is_bool() {
			return Some(Self::new_invalid(EvalError::NonBooleanOperand));
		}
		None
	}

	/// Ensures that the stored number is not wider than the constant
	fn normalize(&mut self) {
		let bit_mask = (BigUint::from(1u32) << self.width) - 1u32;
		self.value &= bit_mask;
	}

	fn normalized(mut self) -> Self {
		self.normalize();
		self
	}
}

impl From<EvalError> for NumericConstant {
	fn from(error: EvalError) -> Self {
		Self::new_invalid(error)
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
		Self::new_signed(value.into()) // TODO maybe it's better to have this as 64-bit always?
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

impl From<NumericConstant> for Result<NumericConstant, EvalError> {
	fn from(value: NumericConstant) -> Self {
		if value.is_valid() {
			Ok(value)
		}
		else {
			Err(value.get_error().unwrap())
		}
	}
}

impl TryFrom<NumericConstant> for i64 {
	type Error = EvalError;

	fn try_from(nc: NumericConstant) -> Result<Self, Self::Error> {
		i64::try_from(nc.to_bigint()?).or(Err(EvalError::NarrowEvalRange))
	}	
}

impl TryFrom<NumericConstant> for u64 {
	type Error = EvalError;

	fn try_from(nc: NumericConstant) -> Result<Self, Self::Error> {
		u64::try_from(nc.to_biguint()?).or(Err(EvalError::NarrowEvalRange))
	}	
}

impl HasSignedness for NumericConstant {
	fn signedness(&self) -> SignalSignedness {
		self.signedness
	}
}

impl std::fmt::Display for NumericConstant {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.is_valid() {
			write!(
				f,
				"{}{}{}",
				self.to_dec_str().expect("could not convert to dec str"),
				if self.is_signed() { "s" } else { "u" },
				self.width().expect("could not get width")
			)?;
		}
		else {
			write!(f, "<invalid>")?;
		}

		Ok(())
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
					(None, None) => {},
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

impl PartialEq for NumericConstant {
	fn eq(&self, other: &Self) -> bool {
		match (self.is_valid(), other.is_valid()) {
			(false, false) => true,
			(false, true) => false,
			(true, false) => false,
			(true, true) => {
				assert!(self.is_signed() == other.is_signed());
				if self.is_signed() {
					self.to_bigint().unwrap() == other.to_bigint().unwrap()
				}
				else {
					self.value == other.value
				}
			}
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
			(true, true) => {
				assert!(self.is_signed() == other.is_signed());
				if self.is_signed() {
					self.to_bigint().unwrap().cmp(&other.to_bigint().unwrap())
				}
				else {
					self.value.cmp(&other.value)
				}
			},
		}
	}
}

impl PartialOrd for NumericConstant {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Eq for NumericConstant {}

impl std::ops::Shr for NumericConstant {
	type Output = NumericConstant;

	fn shr(self, rhs: Self) -> Self::Output {
		self.op_shr(rhs)
	}
}

impl std::ops::Shl for NumericConstant {
	type Output = NumericConstant;

	fn shl(self, rhs: Self) -> Self::Output {
		self.op_shl(rhs)
	}
}

impl_binary_constant_op!(Add, add, |lhs: &NumericConstant,
                                    rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;

	let mut result = NumericConstant::from_bigint(
		lhs.to_bigint()? + rhs.to_bigint()?,
		lhs.signedness()?,
		lhs.width()?.max(rhs.width()?) + 1,
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(Sub, sub, |lhs: &NumericConstant,
                                    rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;

	let mut result = NumericConstant::from_bigint(
		lhs.to_bigint()? - rhs.to_bigint()?,
		lhs.signedness()?,
		lhs.width()?.max(rhs.width()?) + 1,
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(Mul, mul, |lhs: &NumericConstant,
                                    rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;

	let mut result = NumericConstant::from_bigint(
		lhs.to_bigint()? * rhs.to_bigint()?,
		lhs.signedness()?,
		lhs.width()? + rhs.width()?,
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(Div, div, |lhs: &NumericConstant,
                                    rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;

	if rhs.is_zero() {
		return Err(EvalError::DivisionByZero);
	}

	let mut result = NumericConstant::from_bigint(
		lhs.to_bigint()? / rhs.to_bigint()?,
		lhs.signedness()?,
		lhs.width()?,
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(Rem, rem, |lhs: &NumericConstant,
                                    rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;

	if rhs.is_zero() {
		return Err(EvalError::DivisionByZero);
	}

	let mut result = NumericConstant::from_bigint(
		lhs.to_bigint()? % rhs.to_bigint()?,
		lhs.signedness()?,
		rhs.width()?,
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(BitAnd, bitand, |lhs: &NumericConstant,
                                          rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let mut result = NumericConstant::new(
		lhs.to_biguint()? & rhs.to_biguint()?,
		lhs.signedness()?,
		lhs.width()?
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(BitOr, bitor, |lhs: &NumericConstant,
                                        rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let mut result = NumericConstant::new(
		lhs.to_biguint()? | rhs.to_biguint()?,
		lhs.signedness()?,
		lhs.width()?
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(BitXor, bitxor, |lhs: &NumericConstant,
                                          rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let mut result = NumericConstant::new(
		lhs.to_biguint()? ^ rhs.to_biguint()?,
		lhs.signedness()?,
		lhs.width()?
	)?;
	result.normalize();
	Ok(result)
});

#[cfg(test)]
mod test {
	use rstest::rstest;
	use super::*;

	// TODO tests for error cases

	fn check_value(nc: NumericConstant, val: i64, width: u64) {
		assert!(matches!(nc.get_error(), None));
		assert_eq!(nc.to_bigint().unwrap(), BigInt::from(val));
		assert_eq!(nc.width().unwrap(), width.into());
	}

	fn check_value_u(nc: NumericConstant, val: u64, width: u64) {
		assert!(matches!(nc.get_error(), None));
		assert_eq!(*nc.to_biguint().unwrap(), BigUint::from(val));
		assert_eq!(nc.width().unwrap(), width.into());
	}

	fn nc(v: i64) -> NumericConstant {
		NumericConstant::new_signed(v.into())
	}

	fn ncu(v: u64) -> NumericConstant {
		NumericConstant::new_unsigned(v.into())
	}

	#[test]
	fn test_bigint_conversions() {
		assert_eq!(
			NumericConstant::new_signed(0.into()).to_bigint().unwrap(),
			BigInt::from(0)
		);
		assert_eq!(
			NumericConstant::new_signed((-4).into()).to_bigint().unwrap(),
			BigInt::from(-4)
		);
		assert_eq!(
			NumericConstant::new_signed(4.into()).to_bigint().unwrap(),
			BigInt::from(4)
		);
	}

	#[rstest]
	#[case(nc(0), nc(0), nc(0), 2)]
	#[case(nc(5), nc(3), nc(8), 5)]
	#[case(ncu(0), ncu(0), ncu(0), 2)]
	#[case(ncu(1024), ncu(1), ncu(1025), 12)]
	#[case(ncu(1024), ncu(1024), ncu(2048), 12)]
	fn test_add(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs + rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(nc(4), nc(7), nc(-3), 5)]
	#[case(nc(10), nc(9), nc(1), 6)]
	#[case(nc(1), nc(1), nc(0), 3)]
	#[case(ncu(1024), ncu(1024), ncu(0), 12)]
	#[case(ncu(0), ncu(1), ncu(3), 2)]
	#[case(ncu(127), ncu(128), ncu(511), 9)]
	fn test_sub(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs - rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(nc(0), nc(0), nc(0), 2)]
	#[case(nc(17), nc(-1), nc(-17), 7)] // 6b * 1b => 7b
	#[case(nc(-1), nc(-1), nc(1), 2)]
	#[case(nc(10), nc(10), nc(100), 10)]
	#[case(ncu(10), ncu(10), ncu(100), 8)]
	fn test_mul(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs * rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(nc(30), nc(3), nc(10), 6)]
	#[case(nc(31), nc(-1), nc(-31), 6)]
	#[case(nc(-31), nc(-1), nc(31), 6)]
	#[case(ncu(255), ncu(2), ncu(127), 8)]
	#[case(ncu(255), ncu(1), ncu(255), 8)]
	#[case(ncu(30), ncu(3), ncu(10), 5)]
	fn test_div(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs / rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(nc(-5), nc(3), nc(-5 % 3), 3)]
	#[case(nc(-5), nc(-3), nc(-5 % -3), 3)]
	#[case(nc(1247), nc(1), nc(0), 2)]
	#[case(nc(1247), nc(1247), nc(0), 12)]
	#[case(ncu(5), ncu(3), ncu(2), 2)]
	#[case(ncu(560), ncu(33), ncu(560 % 33), 6)]
	fn test_rem(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs % rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(0b1010), ncu(2), ncu(0b1000), 4)]
	#[case(ncu(6), ncu(10), ncu(0), 3)]
	#[case(ncu(5), ncu(1), ncu(2), 3)]
	#[case(nc(15), ncu(1), nc(-2), 5)]
	fn test_shl(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_shl(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(1024), ncu(10), ncu(1), 11)]
	#[case(nc(-7), ncu(1), nc(4), 4)]
	#[case(nc(-8), ncu(1), nc(4), 4)]
	#[case(nc(-8), ncu(2), nc(2), 4)]
	#[case(nc(-8), ncu(3), nc(1), 4)]
	#[case(nc(-8), ncu(4), nc(0), 4)]
	fn test_lsr(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_lsr(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(6), ncu(3), ncu(0), 3)]
	#[case(ncu(6), ncu(3), ncu(0), 3)]
	#[case(ncu(1024), ncu(10), ncu(1), 11)]
	#[case(nc(-8), ncu(1), nc(-4), 4)]
	#[case(nc(-8), ncu(2), nc(-2), 4)]
	#[case(nc(-8), ncu(3), nc(-1), 4)]
	#[case(nc(-8), ncu(4), nc(-1), 4)]
	#[case(nc(-8), ncu(5), nc(-1), 4)]
	fn test_shr(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs >> rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(1), ncu(10), ncu(1024), 11)]
	#[case(ncu(213), ncu(7), ncu(213 * 128), 8 + 7)]
	fn test_shl_expand(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_shl_expand(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(nc(10), ncu(50), nc(10), 50)]
	#[case(nc(-14), ncu(500), nc(-14), 500)]
	fn test_ext(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_ext(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(nc(10), ncu(50), nc(10), 50)]
	#[case(nc(-10), ncu(50), nc(-10), 50)]
	#[case(nc(-14), ncu(500), nc(-14), 500)]
	fn test_sext(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_sext(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(nc(-10), ncu(8), nc(22), 8)]
	fn test_zext(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_zext(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(5), ncu(5), ncu(45), 6)]
	#[case(nc(3), nc(3), ncu(8 + 16 + 3), 6)] // TODO not sure, maybe join should preserve sign of the first arg?
	fn test_join(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_join(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(5), ncu(2), ncu(45), 6)]
	fn test_replicate(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_replicate(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(0b1010), ncu(0), ncu(0), 1)]
	#[case(ncu(0b1010), ncu(1), ncu(1), 1)]
	#[case(ncu(0b1010), ncu(2), ncu(0), 1)]
	#[case(ncu(0b1010), ncu(3), ncu(1), 1)]
	#[case(nc(-3), ncu(0), ncu(1), 1)]
	#[case(nc(-3), ncu(1), ncu(0), 1)]
	#[case(nc(-3), ncu(2), ncu(1), 1)]
	fn test_bit_select(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_bit_select(rhs);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(0b110011), ncu(1), ncu(4), ncu(9), 4)]
	#[case(ncu(0b110011), ncu(0), ncu(0), ncu(1), 1)]
	#[case(ncu(0b110011), ncu(2), ncu(2), ncu(0), 1)]
	#[case(ncu(0b110011), ncu(0), ncu(5), ncu(0b110011), 6)]
	#[case(nc(-3), ncu(0), ncu(2), nc(-3), 3)]
	#[case(nc(-3), ncu(0), ncu(1), nc(1), 2)]
	fn test_bus_select(#[case] lhs: NumericConstant, #[case] lsb: NumericConstant, #[case] msb: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs.op_bus_select(lsb, msb);
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(0b1010), ncu(0b1100), ncu(0b1000), 4)]
	#[case(nc(0b1010), nc(0b1100), nc(0b1000), 5)]
	fn test_bitwise_and(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs & rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(0b1010), ncu(0b1100), ncu(0b1110), 4)]
	#[case(nc(0b1010), nc(0b1100), nc(0b1110), 5)]
	fn test_bitwise_or(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs | rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(ncu(0b1010), ncu(0b1100), ncu(0b0110), 4)]
	#[case(nc(0b1010), nc(0b1100), nc(0b0110), 5)]
	fn test_bitwise_xor(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: NumericConstant, #[case] width: u64) {
		let result = lhs ^ rhs;
		assert_eq!(result, expected);
		assert_eq!(result.width().unwrap(), width);
	}

	#[rstest]
	#[case(nc(-1), nc(1), true)]
	#[case(nc(-1), nc(-1), false)]
	#[case(nc(1), nc(1), false)]
	#[case(nc(0), nc(0), false)]
	#[case(nc(-1), nc(0), true)]
	#[case(nc(-1000), nc(-999), true)]
	#[case(nc(-999), nc(-1000), false)]
	#[case(nc(-1024), nc(1024), true)]
	#[case(ncu(1023), ncu(1024), true)]
	fn test_lt(#[case] lhs: NumericConstant, #[case] rhs: NumericConstant, #[case] expected: bool) {
		let result = lhs.op_lt(&rhs);
		assert_eq!(result, expected.into());
		assert_eq!(result.width().unwrap(), 1);
		assert_eq!(result.signedness().unwrap(), SignalSignedness::Unsigned);
	}

	#[test]
	fn test_rel() {
		check_value_u(nc(-1).op_lt(&nc(1)), 1, 1);
		check_value_u(nc(-1).op_lt(&nc(1)), 1, 1);
		check_value_u(nc(1).op_gt(&nc(-1)), 1, 1);
		check_value_u(nc(1).op_ne(&nc(-1)), 1, 1);
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
