pub mod expression;
pub mod expression_ops;
pub mod functional_blocks;
pub mod module;
pub mod signal;
pub mod scope;

use std::mem;
use std::rc::Rc;
use std::collections::HashMap;
pub use scope::{Scope, ScopeRef};
pub use signal::{Signal, SignalRef};
pub use module::Module;

pub use expression::{Expression, BinaryOp, UnaryOp};

pub type SignalId = usize;
pub type ModuleId = usize;
pub type ScopeId = usize;

pub struct Design {
	// TODO use slotmaps here
	modules: HashMap<ModuleId, Rc<Module>>,
	scopes: HashMap<ScopeRef, Scope>,
	signals: HashMap<SignalRef, Signal>,
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
		todo!();
		// self.modules.get(&mr.id)
	}

	// pub fn add_module(&mut self, module: Module) -> ModuleRef {
	// 	let id = self.new_module_id();
	// 	self.modules.insert(id, module);
	// 	ModuleRef {id}
	// }

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

	pub fn new_root_scope(&mut self) -> &mut Scope {
		todo!();
	}

	// pub fn new_scope(&mut self, ScopeRef) 


}
