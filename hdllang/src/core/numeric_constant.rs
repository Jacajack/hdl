use log::debug;
use num_bigint::BigInt;
use serde::{de::value, Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum NumericConstantBase {
	Binary,
	Decimal,
	Hexadecimal,
	Boolean,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash)]
pub struct NumericConstant {
	pub value: BigInt,
	pub width: Option<u32>,
	pub signed: Option<bool>,
	pub base: Option<NumericConstantBase>,
}
impl PartialEq for NumericConstant {
	fn eq(&self, other: &Self) -> bool {
		self.value == other.value
	}
}
impl NumericConstant {
	/// Creates a new numeric constant from u64
	pub fn from_u64(value: u64, width: Option<u32>, signed: Option<bool>, base: Option<NumericConstantBase>) -> Self {
		let num = Self {
			value: BigInt::from(value),
			width,
			signed,
			base,
		};
		assert!(num.consistency_check());
		num
	}
	pub fn new_from_value(value: BigInt) -> Self {
		let num = Self {
			value,
			width: None,
			signed: None,
			base: None,
		};
		assert!(num.consistency_check());
		num
	}
	pub fn new(value: BigInt, width: Option<u32>, signed: Option<bool>, base: Option<NumericConstantBase>) -> Self {
		let num = Self {
			value,
			width,
			signed,
			base,
		};
		debug!("Created numeric constant: {:?}", num);
		assert!(num.consistency_check());
		num
	}

	pub fn new_from_unary(other: Option<Self>, operation: fn(BigInt) -> BigInt) -> Option<Self> {
		if other.is_none() {
			return None;
		}
		let other = other.clone().unwrap();
		let num = Self {
			value: operation(other.value),
			width: other.width,
			signed: other.signed,
			base: other.base,
		};
		debug!("Created numeric constant: {:?}", num);
		assert!(num.consistency_check());
		Some(num)
	}

	pub fn new_from_binary(
		other1: Option<Self>,
		other2: Option<Self>,
		operation: fn(BigInt, BigInt) -> BigInt,
	) -> Option<Self> {
		if other1.is_none() || other2.is_none() {
			return None;
		}
		let other1 = other1.clone().unwrap();
		let other2 = other2.clone().unwrap();
		let num = Self {
			value: operation(other1.value, other2.value),
			width: other1.width,
			signed: if let Some(s1) = other1.signed {
				Some(s1 || other2.signed.unwrap_or(false))
			}
			else {
				other2.signed
			},
			base: other1.base,
		};
		debug!("Created numeric constant: {:?}", num);
		assert!(num.consistency_check());
		Some(num)
	}

	pub fn new_true() -> Self {
		Self::from_u64(1, Some(1), Some(false), Some(NumericConstantBase::Boolean))
	}

	pub fn new_false() -> Self {
		Self::from_u64(0, Some(1), Some(false), Some(NumericConstantBase::Boolean))
	}
	fn count_ones(&self) -> u32 {
		let mut ones = 0;
		for d in self.value.iter_u32_digits() {
			ones += d.count_ones();
		}
		ones
	}
	/// Internal consistency checks
	fn consistency_check(&self) -> bool {
		// Boolean constants
		if matches!(self.base, Some(NumericConstantBase::Boolean))
			&& self.value != BigInt::from(0u32)
			&& self.value != BigInt::from(1u32)
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
	/// number of bits and signedness (accounts for the sign bit)
	/// ```
	/// use hdllang::core::NumericConstant;
	/// let c = NumericConstant::from_u64(7, Some(4), Some(true), None);
	/// assert_eq!(c.is_representable_as_positive(), Some(true));
	/// assert_eq!(c.is_representable_as_negative(), Some(true));
	/// 
	/// let c = NumericConstant::from_u64(8, Some(4), Some(true), None);
	/// assert_eq!(c.is_representable_as_positive(), Some(false));
	/// assert_eq!(c.is_representable_as_negative(), Some(true));
	/// ````
	pub fn is_representable_as_positive(&self) -> Option<bool> {
		self.get_effective_bits().map(|n| self.value.bits() as u32 <= n)
	}

	/// Returns if the number can be represented with the specifed
	/// number of bits and signedness when negated (this is always false for unsigned numbers)
	pub fn is_representable_as_negative(&self) -> Option<bool> {
		self.get_effective_bits().map(|n| {
			self.signed.unwrap() && (self.value.bits() as u32 <= n || (self.value.bits() as u32 == n + 1 && self.count_ones() as u32 == 1))
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
			return if self.count_ones() == 0 {
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
			self.value.to_str_radix(radix),
			self.signed.map_or("", |s| if s { "s" } else { "u" }),
			self.width.map_or("".to_string(), |w| w.to_string())
		)
	}
}
