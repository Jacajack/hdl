use super::SignalRef;

pub struct Register {
	pub input_nreset: Option<SignalRef>,
	pub input_en: Option<SignalRef>,
	pub input_clk: SignalRef,
	pub input_next: SignalRef,
	pub output_data: SignalRef,
}

pub struct TristateBuffer {
	pub input_en: Option<SignalRef>,
	pub input_data: SignalRef,
	pub output_data: SignalRef,
}

pub struct ClockGate {
	pub input_en: SignalRef,
	pub input_clk: SignalRef,
	pub output_clk: SignalRef,
}

pub struct FfSync {
	pub input_nreset: Option<SignalRef>,
	pub input_en: Option<SignalRef>,
	pub input_clk1: SignalRef,
	pub input_clk2: SignalRef,
	pub input_next: SignalRef,
	pub output_data: SignalRef,
}

pub enum BlockInstance {
	Register(Register),
	TristateBuffer(TristateBuffer),
	ClockGate(ClockGate),
	FfSync(FfSync),
	// Module(ModuleInstance), // FIXME
}
