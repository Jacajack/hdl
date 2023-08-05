
use std::mem;
use std::collections::HashMap;

use super::module::Module;

pub type SignalId = usize;
pub type ModuleId = usize;
pub type ScopeId = usize;

pub struct Design {
	modules: HashMap<ModuleId, Module>,
	next_signal_id: usize,
	next_module_id: usize,
	next_scope_id: usize,
}

#[derive(Copy, Clone)]
pub struct ModuleRef {
	id: ModuleId
}

impl Design {
	pub fn get_module(&self, mr: ModuleRef) -> Option<&Module> {
		self.modules.get(&mr.id)
	}

	pub fn add_module(&mut self, module: Module) -> ModuleRef {
		let id = self.new_module_id();
		self.modules.insert(id, module);
		ModuleRef {id}
	}

	pub fn new_signal_id(&mut self) -> SignalId {
		let id = self.next_signal_id;
		self.next_signal_id += 1;
		id
	}

	pub fn new_module_id(&mut self) -> SignalId {
		let id = self.next_module_id;
		self.next_module_id += 1;
		id
	}



}