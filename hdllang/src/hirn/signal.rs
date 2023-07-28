pub enum SignalClass {
	Logic,
	Signed,
	Unsigned,
}

pub struct EdgeSensitivity {
	pub clock_signal: SignalRef,
	pub on_rising: bool,
}

pub struct SensitivityList {
	pub edges: Vec<EdgeSensitivity>,
}

pub enum SignalSensitivity {
	Async,
	Comb(SensitivityList),
	Sync(SensitivityList),
	Clock,
	Const,
}

pub enum ArraySlice {
	Range(usize, usize), // TODO const_expression
	Index(usize), // TODO const_expression
}

pub struct SignalType {
	pub width: u32,  // TODO const_expression
	pub class: SignalClass,
	pub senitivity: SignalSensitivity,
}

pub struct SignalRef {
	pub signal_id: usize,  // TODO const_expression
	pub slices: Vec<ArraySlice>,  // TODO const_expression
	pub bit_range: (usize, usize),  // TODO const_expression
}

pub struct Signal {
	name: String,
	dimensions: Vec<u32>,  // TODO const_expression
	bit_width: u32,  // TODO const_expression
	class: SignalClass,
	sensitivity: SignalSensitivity,
}

pub enum SignalDirection {
	Input,
	Output,
	Tristate,
}

pub struct InterfaceSignal {
	signal: Signal,
	direction: SignalDirection,
}