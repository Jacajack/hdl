pub mod expression;
pub mod expression_ops;
pub mod functional_blocks;
pub mod module;
pub mod signal;
pub mod scope;

pub use scope::Scope;
pub use signal::Signal;
pub use module::Module;
pub use expression::{Expression, BinaryOp, UnaryOp};

use std::rc::{Weak, Rc};
use std::cell::RefCell;

use thiserror::Error;

use self::signal::SignalBuilder;


/// References a module in a design
#[derive(Clone)]
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

	pub fn add_scope(&mut self, scope: Scope) -> ScopeRef {
		let id = self.next_scope_id;
		self.next_scope_id += 1;
		self.scopes.push(scope);
		self.scopes.last_mut().unwrap().set_design(self.weak.clone(), id);
		ScopeRef{id}
	}

	pub fn add_signal(&mut self, signal: Signal) -> SignalRef {
		let id = self.next_signal_id;
		self.next_signal_id += 1;
		self.signals.push(signal);
		self.signals.last_mut().unwrap().set_design(self.weak.clone(), id);
		SignalRef{id}
	}

	pub fn add_module(&mut self, module: Module) -> ModuleRef {
		let id = self.next_module_id;
		self.next_module_id += 1;
		self.modules.push(module);
		self.modules.last_mut().unwrap().set_design(self.weak.clone(), id);
		ModuleRef{id}
	}

	pub fn get_scope_mut(&mut self, scope: ScopeRef) -> Option<&mut Scope> {
		self.scopes.get_mut(scope.id)
	}

	pub fn new_signal(&mut self, scope: ScopeRef) -> SignalBuilder {
		SignalBuilder::new(self.weak.upgrade().unwrap(), scope)
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

	pub fn add_scope(&mut self, scope: Scope) -> ScopeRef {
		self.handle.borrow_mut().add_scope(scope)
	}

	pub fn add_signal(&mut self, signal: Signal) -> SignalRef {
		self.handle.borrow_mut().add_signal(signal)
	}

	pub fn add_module(&mut self, module: Module) -> ModuleRef {
		self.handle.borrow_mut().add_module(module)
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

	#[test]
	pub fn design_basic_test() -> Result<(), DesignError> {
		let mut d = Design::new();
		let sub_scope = d.add_scope(Scope::new());
		let mut main_scope = Scope::new();

		let sig = main_scope.new_signal()?
			.name("super_duper_signal")
			.width(Expression::new_zero())
			.constant()
			.build()?;

		let cond = main_scope.add_conditional_scope(
			Expression::new_zero(),
			d.add_scope(Scope::new()))?;

		Ok(())
	}
}