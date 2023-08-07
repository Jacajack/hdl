use super::Expression;
use super::scope::ScopeRef;

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
	Range(Box<Expression>, Box<Expression>),
	Index(Box<Expression>),
}

pub struct SignalType {
	pub width: Box<Expression>, 
	pub class: SignalClass,
	pub senitivity: SignalSensitivity,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct SignalRef {
	scope: ScopeRef,
	signal_id: usize,
}

pub struct SignalSlice {
	signal: SignalRef,
	slices: Vec<ArraySlice>,
}

pub struct Signal {
	name: String,
	dimensions: Vec<Expression>,
	bit_width: Box<Expression>,
	class: SignalClass,
	sensitivity: SignalSensitivity,
}

pub enum SignalDirection {
	Input,
	Output,
	Tristate,
}

// TODO look at this
pub struct InterfaceSignal {
	signal: Signal,
	direction: SignalDirection,
}

// TODO
impl std::ops::Index<Expression> for SignalRef {
	type Output = SignalRef;

	fn index(&self, index: Expression) -> &Self::Output {
		todo!();
	}
}

// TODO index op on SignalSlice