use derive_more::{Add, Display};
use num_bigint::BigInt;
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};

/// Dynamic-wdith signed integer
/// Backbone of all integer operations in the compiler
#[derive(Clone, Add, Display, Serialize, Deserialize)]
pub struct WideInt {
	value: BigInt,
}

/// Dynamic-wdith unsigned integer
/// Backbone of all integer operations in the compiler
#[derive(Clone, Display, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct WideUint {
	value: BigUint,
}

impl WideUint {
	pub fn from_u64(value: u64) -> Self {
		Self {
			value: BigUint::from(value),
		}
	}

	/// Returns number of bits required to store this number
	pub fn bits_required(&self) -> u32 {
		self.value.bits() as u32
	}

	/// Returns number of 1 bits
	pub fn count_ones(&self) -> u32 {
		self.value.count_ones() as u32
	}
}
