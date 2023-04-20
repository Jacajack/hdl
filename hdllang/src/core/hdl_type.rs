// pub enum SignalType {
// 	Auto(AutoType),
// 	Bus(BusType),
// 	Wire(WireType),
// 	Int(IntType),
// 	Bool(BoolType),
// }

// pub struct AutoType {
// 	signed: Option<bool>,
// 	sensitivity: Option<Sensitivity>,
// }

// pub struct IntType {
// }

// pub struct BoolType {
// }

// pub struct BusType {
// 	width: Option<u32>,
// 	signed: Option<bool>,
// 	sensitivity: Option<Sensitivity>,
// }

// pub struct WireType {
// 	sensitivity: Option<Sensitivity>,
// }

#[derive(Copy, Clone, PartialEq)]
pub enum Sensitivity {
	Async,
	Comb(ClockSource),
	Sync(ClockSource),
	Clock,
	Const,
}

#[derive(Copy, Clone, PartialEq)]
pub enum ClockSource {
	// Vector of signal IDs
}

pub trait HdlType {
	fn get_sensitivity(&self) -> Option<Sensitivity>;
	fn get_width(&self) -> Option<u32>;
	fn is_signed(&self) -> Option<bool>;

	fn get_clock_source(&self) -> Option<ClockSource> {
		use Sensitivity::*;
		match self.get_sensitivity() {
			Some(Comb(clk)) => Some(clk),
			Some(Sync(clk)) => Some(clk),
			_ => None,
		}
	}

	fn is_unsigned(&self) -> Option<bool> {
		self.is_signed().map(|b| !b)
	}

	fn is_const(&self) -> bool {
		self.get_sensitivity().map_or(false, |s| s == Sensitivity::Const)
	}
}

// impl HdlType for IntType {
// 	fn get_sensitivity(&self) -> Option<Sensitivity> {Some(Sensitivity::Const)}
// 	fn get_width(&self) -> Option<u32> {None}
// 	fn is_signed(&self) -> Option<bool> {Some(true)}
// }

// impl HdlType for BoolType {
// 	fn get_sensitivity(&self) -> Option<Sensitivity> {Some(Sensitivity::Const)}
// 	fn get_width(&self) -> Option<u32> {Some(1)}
// 	fn is_signed(&self) -> Option<bool> {Some(false)}
// }

// impl HdlType for BusType {
// 	fn get_sensitivity(&self) -> Option<Sensitivity> {self.sensitivity}
// 	fn get_width(&self) -> Option<u32> {self.width}
// 	fn is_signed(&self) -> Option<bool> {self.signed}
// }

// impl HdlType for WireType {
// 	fn get_sensitivity(&self) -> Option<Sensitivity> {self.sensitivity}
// 	fn get_width(&self) -> Option<u32> {Some(1)}
// 	fn is_signed(&self) -> Option<bool> {Some(false)}
// }

// impl HdlType for SignalType {

// }
