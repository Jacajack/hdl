#[derive(Clone, Copy, Debug)]
pub struct NumericConstant {
	// pub value: WideUint,
	pub value: u64, // FIXME
	pub width: Option<u32>,
	pub signed: Option<bool>,
}

// impl HdlType for NumericConstant {
// 	fn get_sensitivity(&self) -> Option<Sensitivity> {
// 		Some(Sensitivity::Const)
// 	}
// 	fn get_width(&self) -> Option<u32> {
// 		self.width
// 	}
// 	fn is_signed(&self) -> Option<bool> {
// 		self.signed
// 	}
// }


impl NumericConstant {
	/// Creates a new numeric constant from u64
	pub fn from_u64(value: u64, width: Option<u32>, signed: Option<bool>) -> Self {
		Self {value, width, signed}
	}

	/// Returns true if the constant is fully constrained in terms
	/// of width and signedness
	pub fn is_fully_constrained(&self) -> bool {
		self.width.is_some() && self.signed.is_some()
	}

	/// Returns number of effective bits (n-1 if signed)
	fn get_effective_bits(&self) -> u32 {
		assert!(self.is_fully_constrained());
		self.width.unwrap() - (if self.signed.unwrap() {1} else {0})
	}

	/// Checks whether width is in valid range
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
	pub fn is_representable_as_positive(&self) -> bool {
		let n = self.get_effective_bits();
		self.value <= u64::pow(2u64, n) - 1
	}

	/// Returns if the number can be represented with the specifed
	/// number of bits and signedness when negated
	pub fn is_representable_as_negative(&self) -> bool {
		let n = self.get_effective_bits();
		self.value <= u64::pow(2u64, n)
	}
}
