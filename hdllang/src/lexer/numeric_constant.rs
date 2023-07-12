use crate::core::WideUint;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum NumericConstantBase {
	Binary,
	Decimal,
	Hexadecimal,
	Boolean,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NumericConstant {
	pub value: WideUint,
	pub width: Option<u32>,
	pub signed: Option<bool>,
	pub base: Option<NumericConstantBase>,
}

impl NumericConstant {
	/// Creates a new numeric constant from u64
	pub fn from_u64(value: u64, width: Option<u32>, signed: Option<bool>, base: Option<NumericConstantBase>) -> Self {
		let num = Self {
			value: WideUint::from_u64(value),
			width,
			signed,
			base,
		};
		assert!(num.consistency_check());
		num
	}

	/// Internal consistency checks
	fn consistency_check(&self) -> bool {
		// Boolean constants
		if matches!(self.base, Some(NumericConstantBase::Boolean))
			&& self.value != WideUint::from_u64(0)
			&& self.value != WideUint::from_u64(1)
		{
			return false;
		}

		// Id width is specified, signedness must be specified as well
		if self.width.is_some() && self.signed.is_none() {
			return false;
		}

		// Check if width is valid
		if !self.is_width_valid() {
			return false;
		}

		true
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

	/// Converts the numeric constant to a HIRL string
	/// Used for pretty printer
	pub fn to_pretty_string(&self) -> String {
		assert!(self.consistency_check());
		use NumericConstantBase::*;

		// Special case for boolean constants
		if matches!(self.base, Some(Boolean)) {
			return if self.value.count_ones() == 0 {
				"false".to_string()
			}
			else {
				"true".to_string()
			};
		}

		let radix_prefix;
		let radix;
		match self.base {
			Some(Binary) => {
				radix_prefix = "0b";
				radix = 2;
			},
			Some(Decimal) | None => {
				radix_prefix = "";
				radix = 10;
			},
			Some(Hexadecimal) => {
				radix_prefix = "0x";
				radix = 16;
			},
			Some(Boolean) => unreachable!(),
		};

		format!(
			"{}{}{}{}",
			radix_prefix,
			self.value.to_string_radix(radix),
			self.signed.map_or("", |s| if s { "s" } else { "u" }),
			self.width.map_or("".to_string(), |w| w.to_string())
		)
	}
}
