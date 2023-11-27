use std::cmp::max;

use super::{NarrowEval, WidthExpression};
use crate::design::{Expression, HasSignedness, SignalSignedness};

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
			.or_else(|| Self::require_width_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self < rhs))
	}

	pub fn op_lte(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.or_else(|| Self::require_width_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self <= rhs))
	}

	pub fn op_gt(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.or_else(|| Self::require_width_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self > rhs))
	}

	pub fn op_gte(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.or_else(|| Self::require_width_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self >= rhs))
	}

	pub fn op_eq(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.or_else(|| Self::require_width_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self == rhs))
	}

	pub fn op_ne(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.or_else(|| Self::require_width_match(self, rhs))
			.unwrap_or_else(|| Self::new_bool(self != rhs))
	}

	pub fn op_min(&self, rhs: &NumericConstant) -> Self {
		Self::propagate_err(self, rhs)
			.or_else(|| Self::require_sign_match(self, rhs))
			.or_else(|| Self::require_width_match(self, rhs))
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
			.or_else(|| Self::require_width_match(self, rhs))
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
				if n > self.width {
					Self::new_invalid(EvalError::BadBitSelect)
				}
				else {
					Self::new_bool(self.value.bit(n))
				}
			},
		}
	}

	fn op_bus_select_i(&self, lsb: u64, msb: u64) -> Self {
		if msb < lsb {
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

macro_rules! impl_rust_binary_constant_op {
	($trait_name: ident, $trait_func: ident, $operator: tt) => {
		impl_binary_constant_op!($trait_name, $trait_func, |lhs: &NumericConstant, rhs: &NumericConstant| -> Result<NumericConstant, EvalError> {
			check_sign_match(lhs, rhs)?;
			let width_expr = Expression::from(lhs.clone()) $operator rhs.clone().into();
			let mut result = NumericConstant::from_bigint(
				lhs.to_bigint()? $operator rhs.to_bigint()?,
				lhs.signedness()?,
				width_expr.width()?.const_narrow_eval()? as u64
			)?;
			result.normalize();
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
			(true, true) => {
				assert!(self.is_signed() == other.is_signed());
				assert!(self.width == other.width);
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

impl_rust_binary_constant_op!(Add, add, +);
impl_rust_binary_constant_op!(Sub, sub, -);
impl_rust_binary_constant_op!(Mul, mul, *);

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

impl_binary_constant_op!(Div, div, |lhs: &NumericConstant,
                                    rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;

	if rhs.is_zero() {
		return Err(EvalError::DivisionByZero);
	}

	let width_expr = Expression::from(lhs.clone()) / rhs.clone().into();
	let mut result = NumericConstant::from_bigint(
		lhs.to_bigint()? / rhs.to_bigint()?,
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64,
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

	let width_expr = Expression::from(lhs.clone()) % rhs.clone().into();
	let mut result = NumericConstant::from_bigint(
		lhs.to_bigint()? % rhs.to_bigint()?,
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64,
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(BitAnd, bitand, |lhs: &NumericConstant,
                                          rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let width_expr = Expression::from(lhs.clone()) & rhs.clone().into();
	let mut result = NumericConstant::new(
		lhs.to_biguint()? & rhs.to_biguint()?,
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64,
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(BitOr, bitor, |lhs: &NumericConstant,
                                        rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let width_expr = Expression::from(lhs.clone()) | rhs.clone().into();
	let mut result = NumericConstant::new(
		lhs.to_biguint()? | rhs.to_biguint()?,
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64,
	)?;
	result.normalize();
	Ok(result)
});

impl_binary_constant_op!(BitXor, bitxor, |lhs: &NumericConstant,
                                          rhs: &NumericConstant|
 -> Result<NumericConstant, EvalError> {
	check_sign_match(lhs, rhs)?;
	check_width_match(lhs, rhs)?;
	let width_expr = Expression::from(lhs.clone()) ^ rhs.clone().into();
	let mut result = NumericConstant::new(
		lhs.to_biguint()? ^ rhs.to_biguint()?,
		lhs.signedness()?,
		width_expr.width()?.const_narrow_eval()? as u64,
	)?;
	result.normalize();
	Ok(result)
});

#[cfg(test)]
mod test {
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
		check_value(nc(17) * nc(-1), -17, 7); // 6b * 1b => 7b
		check_value(nc(-1) * nc(-1), 1, 2);
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
	fn test_shift_logic() {
		check_value_u(ncu(0b1010) << ncu(2), 0b1000, 4);
		check_value_u(ncu(6) << ncu(10), 0, 3);
		check_value_u(ncu(6) >> ncu(3), 0, 3);
		check_value_u(ncu(6) >> ncu(3), 0, 3);
		check_value_u(ncu(5) << ncu(1), 2, 3);
		check_value_u(ncu(1024) >> ncu(10), 1, 11);

		check_value(nc(-7).op_lsr(ncu(1)), 4, 4);
		check_value(nc(15) << ncu(1), -2, 5);
	}

	#[test]
	fn test_shift_expand() {
		check_value_u(ncu(1).op_shl_expand(ncu(10)), 1024, 11);
		check_value_u(ncu(213).op_shl_expand(ncu(7)), 213 * 128, 8 + 7);
	}

	#[test]
	fn test_shift_arith() {
		check_value(nc(-8) >> ncu(1), -4, 4);
		check_value(nc(-8) >> ncu(2), -2, 4);
		check_value(nc(-8) >> ncu(3), -1, 4);
		check_value(nc(-8) >> ncu(4), -1, 4);
		check_value(nc(-8) << ncu(1), 0, 4);
	}

	#[test]
	fn test_ext() {
		check_value(nc(10).op_ext(ncu(50)), 10, 50);
		check_value(nc(-10).op_ext(ncu(50)), -10, 50);
		check_value(nc(-10).op_sext(ncu(50)), -10, 50);
		check_value(nc(-10).op_zext(ncu(8)), 22, 8);
	}

	#[test]
	fn test_join() {
		check_value(ncu(5).op_join(ncu(5)), 45, 6);
	}

	#[test]
	fn test_replicate() {
		check_value(ncu(5).op_replicate(ncu(2)), 45, 6);
	}

	#[test]
	fn test_bit_select() {
		check_value_u(ncu(0b1010).op_bit_select(ncu(0)), 0, 1);
		check_value_u(ncu(0b1010).op_bit_select(ncu(1)), 1, 1);
		check_value_u(ncu(0b1010).op_bit_select(ncu(2)), 0, 1);
		check_value_u(ncu(0b1010).op_bit_select(ncu(3)), 1, 1);
	}

	#[test]
	fn test_bus_select() {
		check_value_u(ncu(0b110011).op_bus_select(ncu(1), ncu(4)), 9, 4);
		check_value_u(ncu(0b110011).op_bus_select(ncu(0), ncu(0)), 1, 1);
		check_value_u(ncu(0b110011).op_bus_select(ncu(2), ncu(2)), 0, 1);
		check_value_u(ncu(0b110011).op_bus_select(ncu(0), ncu(5)), 0b110011, 6);
	}

	#[test]
	fn test_bitwise_and() {
		check_value_u(ncu(0b1010) & ncu(0b1100), 0b1000, 4);
	}

	#[test]
	fn test_bitwise_or() {
		check_value_u(ncu(0b1010) | ncu(0b1100), 0b1110, 4);
	}

	#[test]
	fn test_bitwise_xor() {
		check_value_u(ncu(0b1010) ^ ncu(0b1100), 0b0110, 4);
	}

	// FIXME enable back after merging #309 - this fails due to overly restrictive
	// width rules, which has already been relaxed on that branch.
	// #[test]
	// fn test_rel() {
	// 	check_value_u(nc(-1).op_lt(&nc(1)), 1, 1);
	// 	check_value_u(nc(-1).op_lt(&nc(1)), 1, 1);
	// 	check_value_u(nc(1).op_gt(&nc(-1)), 1, 1);
	// 	check_value_u(nc(1).op_ne(&nc(-1)), 1, 1);
	// }

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
