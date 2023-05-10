use crate::core::WideUint;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NumericConstant {
	pub value: WideUint,
	pub width: Option<u32>,
	pub signed: Option<bool>,
}

impl NumericConstant {
	/// Creates a new numeric constant from u64
	pub fn from_u64(value: u64, width: Option<u32>, signed: Option<bool>) -> Self {
		Self {
			value: WideUint::from_u64(value),
			width,
			signed,
		}
	}

	/// Returns true if the constant is fully constrained in terms
	/// of width and signedness
	pub fn is_fully_constrained(&self) -> bool {
		self.width.is_some() && self.signed.is_some()
	}

	/// Returns number of effective bits (n-1 if signed)
	fn get_effective_bits(&self) -> Option<u32> {
		if self.is_fully_constrained() && self.is_width_valid() {
			Some(self.width.unwrap() - (if self.signed.unwrap() { 1 } else { 0 }))
		}
		else {
			None
		}
	}

	/// Checks whether width is in supported range
	/// (does not check if the value is representable)
	fn is_width_valid(&self) -> bool {
		if let Some(n) = self.width {
			n > 0 && n <= 64
		}
		else {
			true
		}
	}

	/// Returns if the number can be represented with the specifed
	/// number of bits and signedness
	pub fn is_representable_as_positive(&self) -> Option<bool> {
		self.get_effective_bits().map(|n| self.value.bits_required() <= n)
	}

	/// Returns if the number can be represented with the specifed
	/// number of bits and signedness when negated
	pub fn is_representable_as_negative(&self) -> Option<bool> {
		self.get_effective_bits().map(|n| {
			self.value.bits_required() <= n || (self.value.bits_required() == n + 1 && self.value.count_ones() == 1)
		})
	}

	/// Returns true if the number is always representable
	/// regardless whether it's negated or not
	pub fn is_always_representable(&self) -> Option<bool> {
		match (self.is_representable_as_positive(), self.is_representable_as_negative()) {
			(Some(x), Some(y)) => Some(x && y),
			_ => None,
		}
	}

	/// Checks if the number is representable as positive OR as negative
	pub fn is_representable(&self) -> Option<bool> {
		match (self.is_representable_as_positive(), self.is_representable_as_negative()) {
			(Some(x), Some(y)) => Some(x || y),
			_ => None,
		}
	}
	pub fn to_string(&self) -> String {
		self.value.to_string()
	}
}
