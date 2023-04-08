use num_bigint::BigUint;
use num_bigint::BigInt;
use derive_more::{Add, Display};

/// Dynamic-wdith signed integer
/// Backbone of all integer operations in the compiler
#[derive(Clone, Add, Display)]
pub struct WideInt {
	value: BigInt
}

/// Dynamic-wdith unsigned integer
/// Backbone of all integer operations in the compiler
#[derive(Clone, Copy)]
pub struct WideUint {


}