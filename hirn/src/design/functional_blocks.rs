use super::{ModuleId, Expression, SignalSlice};

/// Register block
pub struct Register {
	/// Asynchronous negated reset input
	pub input_nreset: Option<Expression>,

	/// Enable input
	pub input_en: Option<Expression>,

	/// Clock input
	pub input_clk: SignalSlice,

	/// Next value input
	pub input_next: Expression,

	/// Output value
	pub output_data: SignalSlice,
}

/// Tristate buffer block
pub struct TristateBuffer {
	/// Enable input
	pub input_en: Option<Expression>,

	/// Data input
	pub input_data: Expression,

	/// Output data
	pub output_data: SignalSlice,
}

/// Clock gating block
pub struct ClockGate {
	/// Enable input
	pub input_en: Expression,

	/// Clock input
	pub input_clk: SignalSlice,

	/// Output clock
	pub output_clk: SignalSlice,
}

// FF synchronizer block
pub struct FfSync {
	/// Asynchronous negated reset input
	pub input_nreset: Option<Expression>,

	/// Enable input
	pub input_en: Option<Expression>,

	/// Input clock signal input
	pub input_clk1: SignalSlice,

	/// Output clock signal input
	pub input_clk2: SignalSlice,

	/// Next value input
	pub input_next: Expression,
	
	/// Output value
	pub output_data: SignalSlice,
}

/// Represents an instantiated module
pub struct ModuleInstance {
	/// ID of the instantiated module
	pub module: ModuleId,
	// TODO binding list
}

/// Represents a functional block instance
pub enum BlockInstance {
	Register(Register),
	TristateBuffer(TristateBuffer),
	ClockGate(ClockGate),
	FfSync(FfSync),
	Module(ModuleInstance),
}
