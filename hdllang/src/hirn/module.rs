use super::signal::{SignalRef, SignalType, Signal};
use super::functional_blocks::BlockInstance;
use super::Expression;

pub struct GenericRef {

}

pub struct GenericParam {
	pub name: String,
	pub value: Option<i64>,
}

pub struct GenericVariable {

}

pub struct Module {
	pub namespace_path: Vec<String>,
	pub name: String,
	pub interface: Scope,
	pub main_scope: Scope,
	pub parameters: Vec<GenericParam>,
}

impl Module {
	pub fn new(name: String, namespace_path: Vec<String>) -> Self {
		Self {
			namespace_path,
			name,	
			interface: Scope::new(),
			main_scope: Scope::new(), // TODO link scopes
			parameters: Vec::new(),
		}
	}

	pub fn add_generic_parameter(&mut self, name: &String) {
		self.parameters.push(
			GenericParam {
				name: name.clone(),
				value: None,
			}
		);
	}

	pub fn add_interface_signal(&mut self, name: &String, signal_type: SignalType) {
		todo!();
	}

	pub fn get_scope(&mut self) -> &mut Scope {
		todo!();
	}
}

pub struct ModuleInstance {
	pub name: String,
	pub bindings: Vec<(String, SignalRef)>,
	// TODO generics
}

pub struct ConditionalScope {
	pub condition: Expression,
	pub scope: Scope,
}

pub struct RangeScope {
	pub iterator_var: GenericRef,
	pub iterator_begin: Expression,
	pub iterator_end: Expression,
	pub scope: Scope,
}

pub struct Assignment {
	pub lhs: SignalRef,
	pub rhs: Expression,
}

pub struct Scope {
	signals: Vec<Signal>,
	generic: Vec<GenericVariable>,
	assignments: Vec<Assignment>,
	loops: Vec<RangeScope>,
	conditionals: Vec<ConditionalScope>,
	blocks: Vec<BlockInstance>,
}

impl Scope {
	pub fn new() -> Self {
		Self {
			signals: vec![],
			generic: vec![],
			assignments: vec![],
			loops: vec![],
			conditionals: vec![],
			blocks: vec![],
		}
	}

	pub fn get_signal_ref(&mut self, name: &String) -> SignalRef {
		todo!();
	}

	pub fn add_signal(&mut self, signal_type: SignalType, name: String) {
		todo!();
	}

	pub fn assign(&mut self, lhs: SignalRef, rhs: Expression) {
		todo!();
	}

	pub fn conditional_block(&mut self, condition: &Expression) -> &mut Scope {
		todo!();
	}

	pub fn loop_block(&mut self, iterator: GenericRef, from: Expression, to: Expression) -> &mut Scope {
		todo!();
	}

	pub fn subscope(&mut self) -> &mut Scope {
		todo!();
	}
	
	// TODO add_register
	// TODO add_tristate_buffer
	// TODO add_clock_gate
	// TODO add_ff_sync
	// TODO add_instance

}