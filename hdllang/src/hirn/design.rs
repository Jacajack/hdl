pub mod expression;
pub mod expression_ops;
pub mod functional_blocks;
pub mod module;
pub mod signal;
pub mod scope;

use log::debug;
pub use scope::{Scope, ScopeHandle};
pub use signal::{Signal, SignalClass};
pub use module::Module;
pub use expression::{Expression, BinaryOp, UnaryOp};

use std::rc::{Weak, Rc};
use std::cell::RefCell;

use thiserror::Error;

use self::module::ModuleHandle;
use self::signal::SignalBuilder;


/// References a module in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ModuleRef {
	id: usize,
}

impl ModuleRef {
	pub fn is_null(&self) -> bool {
		self.id == 0
	}
}

/// References a signal in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct SignalRef {
	id: usize,
}

impl SignalRef {
	pub fn is_null(&self) -> bool {
		self.id == 0
	}
}

/// References a scope in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ScopeRef {
	id: usize,
}

impl ScopeRef {
	pub fn is_null(&self) -> bool {
		self.id == 0
	}
}



pub struct DesignCore {
	weak: WeakDesignHandle,
	modules: Vec<Module>,
	scopes: Vec<Scope>,
	signals: Vec<Signal>,
	next_module_id: usize,
	next_scope_id: usize,
	next_signal_id: usize,
}

impl DesignCore {
	pub fn new() -> Self {
		Self {
			weak: WeakDesignHandle::new(),
			modules: Vec::new(),
			scopes: Vec::new(),
			signals: Vec::new(),
			next_module_id: 1,
			next_scope_id: 1,
			next_signal_id: 1,
		}
	}

	fn new_scope(&mut self) -> ScopeHandle {
		self.add_scope(Scope::new())
	}

	fn add_scope(&mut self, scope: Scope) -> ScopeHandle {
		let id = self.next_scope_id;
		self.next_scope_id += 1;
		self.scopes.push(scope);
		self.scopes.last_mut().unwrap().id = ScopeRef{id};
		ScopeHandle::new(self.weak.upgrade().unwrap(), ScopeRef{id})
	}

	fn add_signal(&mut self, signal: Signal) -> SignalRef {
		let id = self.next_signal_id;
		self.next_signal_id += 1;
		self.signals.push(signal);
		self.signals.last_mut().unwrap().id = SignalRef{id};
		// TODO assert scope collisions
		SignalRef{id}
	}

	fn add_module(&mut self, module: Module) -> ModuleHandle {
		let id = self.next_module_id;
		self.next_module_id += 1;
		self.modules.push(module);
		self.modules.last_mut().unwrap().id = ModuleRef{id};
		// TODO assert name conflicts
		debug!("Added module with ID {}", id);
		ModuleHandle::new(self.weak.upgrade().unwrap(), ModuleRef{id})
	}

	fn get_scope_mut(&mut self, scope: ScopeRef) -> Option<&mut Scope> {
		self.scopes.get_mut(scope.id - 1)
	}

	fn get_module_mut(&mut self, module: ModuleRef) -> Option<&mut Module> {
		self.modules.get_mut(module.id - 1)
	}

	pub fn new_module(&mut self, name: String) -> Result<ModuleHandle, DesignError> {
		let main_scope = self.new_scope();
		let module = Module::new(name, vec![], main_scope.id());
		debug!("Creating module with scope ID {}", main_scope.id().id);
		Ok(self.add_module(module))
	}


}

pub type WeakDesignHandle = Weak<RefCell<DesignCore>>;
pub type DesignHandle = Rc<RefCell<DesignCore>>;


pub struct Design {
	handle: DesignHandle
}

impl Design {
	pub fn new() -> Self {
		let d = Self {
			handle: Rc::new(RefCell::new(DesignCore::new()))
		};

		d.handle.borrow_mut().weak = Rc::downgrade(&d.handle);
		d
	}

	pub fn new_module(&mut self, name: String) -> Result<ModuleHandle, DesignError> {
		self.handle.borrow_mut().new_module(name)
	}

}

#[derive(Clone, Copy, Debug, Error)]
pub enum DesignError {
	#[error("This object is not part of any HIRN Design.")]
	NotInDesign,

	#[error("Provided scope is not part of any HIRN design.")]
	ScopeNotInDesign,

	#[error("Provided scope already has a parent scope assigned.")]
	ScopeAlreadyOwned,

	#[error("Invalid name")]
	InvalidName,

	#[error("Signal width not specified")]
	SignalWidthNotSpecified,

	#[error("Signal class not specified")]
	SignalClassNotSpecified,

	#[error("Signal sensitivity not specified")]
	SignalSensitivityNotSpecified,
}

#[cfg(test)]
mod test {
	use super::*;

	// fn init() {
    //     let _ = env_logger::builder().is_test(true).try_init();
    // }

	#[test]
	pub fn design_basic_test() -> Result<(), DesignError> {
		// init();
		let mut d = Design::new();
		let mut m = d.new_module("test".to_string())?;

		let sig = m.scope().new_signal()?
			.name("test_signal")
			.width(Expression::new_zero())
			.class(SignalClass::Logic)
			.constant()
			.build()?;

		let sig2 = m.scope().new_signal()?
			.name("test_signal")
			.width(sig.into())
			.class(SignalClass::Logic)
			.constant()
			.build()?;

		let mut scope2 = m.scope().if_scope(Expression::new_zero())?;

		Ok(())
	}
}