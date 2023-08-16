use super::{ModuleId, Expression, SignalSlice, ScopeHandle, DesignError};

/// Register block
pub struct Register {
	/// Asynchronous negated reset input
	pub input_nreset: Expression,

	/// Enable input
	pub input_en: Expression,

	/// Clock input
	pub input_clk: SignalSlice,

	/// Next value input
	pub input_next: Expression,

	/// Output value
	pub output_data: SignalSlice,
}

#[derive(Clone, Copy, Debug)]
pub enum ReqiuredRegisterSignal {
	Nreset,
	Clk,
	Next,
	Output,
}

/// A builder for register blocks
pub struct RegisterBuilder {
	scope: ScopeHandle,
	input_nreset: Option<Expression>,
	input_en: Option<Expression>,
	input_clk: Option<SignalSlice>,
	input_next: Option<Expression>,
	output_data: Option<SignalSlice>,
}

impl RegisterBuilder {
	pub fn new(scope: ScopeHandle) -> Self {
		Self {
			scope,
			input_nreset: None,
			input_en: None,
			input_clk: None,
			input_next: None,
			output_data: None,
		}
	}

	/// Connects a negated reset signal
	pub fn nreset(mut self, expr: Expression) -> Self {
		assert!(self.input_nreset.is_none());
		self.input_nreset = Some(expr);
		self	
	}

	/// Connects an enable signal
	pub fn en(mut self, expr: Expression) -> Self {
		assert!(self.input_en.is_none());
		self.input_en = Some(expr);
		self
	}

	/// Connects a clock signal
	pub fn clk(mut self, signal: SignalSlice) -> Self {
		assert!(self.input_clk.is_none());
		self.input_clk = Some(signal);
		self
	}

	/// Connects a next value signal
	pub fn next(mut self, expr: Expression) -> Self {
		assert!(self.input_next.is_none());
		self.input_next = Some(expr);
		self
	}

	/// Connects an output signal
	pub fn output(mut self, signal: SignalSlice) -> Self {
		assert!(self.output_data.is_none());
		self.output_data = Some(signal);
		self
	}

	/// Creates the register
	pub fn build(mut self) -> Result<(), DesignError> {
		// TODO assert clock is a clock
		// TODO assert input is comb to this clock
		// TODO assert output is sync to this clock
		// TODO assert input and output have same width
		// TODO assert nreset is 1-bit
		// TODO assert en is 1-bit and comb to specified clock

		use ReqiuredRegisterSignal::*;
		self.scope.add_block(BlockInstance::Register(Register{
			input_clk: self.input_clk.ok_or(DesignError::RequiredRegisterSignalNotConnected(Clk))?,
			input_en: self.input_en.unwrap_or(Expression::new_one()),
			input_nreset: self.input_nreset.ok_or(DesignError::RequiredRegisterSignalNotConnected(Nreset))?,
			input_next: self.input_next.ok_or(DesignError::RequiredRegisterSignalNotConnected(Next))?,
			output_data: self.output_data.ok_or(DesignError::RequiredRegisterSignalNotConnected(Output))?,
		}))
	}
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
