use crate::core::{HdlType, Sensitivity, WideUint};

#[derive(Clone, Copy)]
pub struct NumericConstant {
	pub value: WideUint,
	pub width: Option<u32>,
	pub signed: Option<bool>,
}

impl HdlType for NumericConstant {
	fn get_sensitivity(&self) -> Option<Sensitivity> {
		Some(Sensitivity::Const)
	}
	fn get_width(&self) -> Option<u32> {
		self.width
	}
	fn is_signed(&self) -> Option<bool> {
		self.signed
	}
}

impl NumericConstant {
	/// Parses a string and returns a numeric constant
	pub fn from_str(s: &str) -> Self {
		Self {
			value: WideUint {},
			width: None,
			signed: None,
		}
	}
}
