pub struct HirnDesign {
	pub root_namespace: Namespace,
}

pub struct Namespace {
	pub name: String,
	pub namespaces: Vec<Namespace>,
	pub modules: Vec<Module>,
}

pub struct NumericConstant {
	pub width: u32,
	pub class: SignalClass,
	pub value: Vec<u8>,
}

pub struct Module {
	pub name: String,
	pub interface: Scope,
	pub parameters: Vec<GenericParam>,
}



pub struct GenerateIf {

}

pub struct GenerateFor {
	pub scope: Scope,
	pub iterator_var: GenericRef,
	pub iterator_begin: GenericRef,
	pub iterator_end: GenericRef,
}

pub struct Assignment {

}

pub struct ModuleInstance {
	pub name: String,
	// TODO bindings
}

pub struct Register {
	pub width: u32,
	pub class: SignalClass,
	pub input_nreset: Option<SignalRef>,
	pub input_clk: SignalRef,
	pub input_next: SignalRef,
	pub output_data: SignalRef,

}

pub struct Scope {
	// signals
	// genvars
}


pub struct GenericParam {
	pub name: String,

}

pub struct SignalType {
	pub width: u32,
	pub class: SignalClass,
	pub senitivity: SignalSensitivity,
}

pub enum SignalClass {
	Logic,
	Signed,
	Unsigned,
}

pub enum SignalSensitivity {
	Asynchronous,
	Combinational,
	Sequential,
	Clock,
}

pub enum Operation {
	ConditionalChain,
	Constant{value: NumericConstant},
	SignalRef{signal: SignalRef},
	Addition{lhs: Box<Operation>, rhs: Box<Operation>},
	Subtraction{lhs: Box<Operation>, rhs: Box<Operation>},
	Cast{dest_type: SignalType, src: Box<Operation>},
}