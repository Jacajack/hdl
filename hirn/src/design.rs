pub mod expression;
pub mod expression_ops;
pub mod functional_blocks;
pub mod module;
pub mod signal;
pub mod scope;

pub use scope::{Scope, ScopeHandle};
pub use signal::{Signal, SignalClass, SignalSlice};
pub use module::{Module, ModuleHandle};
pub use expression::{Expression, BinaryOp, UnaryOp};
pub use functional_blocks::{Register, RegisterBuilder};

use log::debug;
use std::rc::{Weak, Rc};
use std::cell::RefCell;
use thiserror::Error;

/// References a module in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ModuleId {
	id: usize,
}

impl ModuleId {
	/// Checks if the reference is valid
	pub fn is_null(&self) -> bool {
		self.id == 0
	}
}

/// References a signal in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct SignalId {
	id: usize,
}

/// References a signal in a design
impl SignalId {
	/// Checks if the reference is valid
	pub fn is_null(&self) -> bool {
		self.id == 0
	}
}

/// References a scope in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ScopeId {
	id: usize,
}

impl ScopeId {
	/// Checks if the reference is valid
	pub fn is_null(&self) -> bool {
		self.id == 0
	}
}

/// Core part of the generic design representation
/// Refferred to via multiple handles with reference
/// counting.
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
	/// Creates a new empty design
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

	/// Creates a new scope and adds it to the design
	fn new_scope(&mut self) -> ScopeHandle {
		self.add_scope(Scope::new())
	}

	/// Adds an existing scope to the design
	fn add_scope(&mut self, scope: Scope) -> ScopeHandle {
		let id = self.next_scope_id;
		self.next_scope_id += 1;
		self.scopes.push(scope);
		self.scopes.last_mut().unwrap().id = ScopeId{id};
		ScopeHandle::new(self.weak.upgrade().unwrap(), ScopeId{id})
	}

	/// Adds an existing signal to the design
	fn add_signal(&mut self, signal: Signal) -> SignalId {
		let id = self.next_signal_id;
		self.next_signal_id += 1;
		self.signals.push(signal);
		self.signals.last_mut().unwrap().id = SignalId{id};
		// TODO assert scope collisions
		SignalId{id}
	}

	/// Adds an existing module to the design
	fn add_module(&mut self, module: Module) -> ModuleHandle {
		let id = self.next_module_id;
		self.next_module_id += 1;
		self.modules.push(module);
		self.modules.last_mut().unwrap().id = ModuleId{id};
		// TODO assert name conflicts
		debug!("Added module with ID {}", id);
		ModuleHandle::new(self.weak.upgrade().unwrap(), ModuleId{id})
	}

	/// Returns a mutable reference to the signal with the given ID
	fn get_scope_mut(&mut self, scope: ScopeId) -> Option<&mut Scope> {
		self.scopes.get_mut(scope.id - 1)
	}

	/// Returns a mutable reference to the module with the given ID
	fn get_module_mut(&mut self, module: ModuleId) -> Option<&mut Module> {
		self.modules.get_mut(module.id - 1)
	}

	/// Creates a new module in the design
	pub fn new_module(&mut self, name: String) -> Result<ModuleHandle, DesignError> {
		let main_scope = self.new_scope();
		let module = Module::new(name, vec![], main_scope.id());
		debug!("Creating module with scope ID {}", main_scope.id().id);
		Ok(self.add_module(module))
	}


}

/// Weak reference to a design
pub type WeakDesignHandle = Weak<RefCell<DesignCore>>;

/// Strong reference to a design
pub type DesignHandle = Rc<RefCell<DesignCore>>;

/// Represents a hardware design
pub struct Design {
	handle: DesignHandle
}

impl Design {
	/// Creates a new HIRN design
	pub fn new() -> Self {
		let d = Self {
			handle: Rc::new(RefCell::new(DesignCore::new()))
		};

		d.handle.borrow_mut().weak = Rc::downgrade(&d.handle);
		d
	}

	/// Creates a new module with provided name and returns a handle to it
	pub fn new_module(&mut self, name: String) -> Result<ModuleHandle, DesignError> {
		self.handle.borrow_mut().new_module(name)
	}
}

/// Represents an error that can occur during design construction.
/// Elaboration errors are not accounted for here.
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

	#[error("Conflicting signal sensitivity")]
	ConflictingSignalSensitivity,

	#[error("Required register signal is not connected")]
	RequiredRegisterSignalNotConnected(functional_blocks::ReqiuredRegisterSignal),
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	pub fn design_basic_test() -> Result<(), DesignError> {
		// init();
		let mut d = Design::new();
		let mut m = d.new_module("test".to_string())?;

		let sig = m.scope().new_signal()?
			.name("test_signal")
			.unsigned(Expression::new_zero())
			.constant()
			.build()?;

		let expr = Expression::from(sig) + sig.into();

		let sig2 = m.scope().new_signal()?
			.name("test_signal")
			.logic(expr.clone())
			.constant()
			.build()?;

		let mut scope2 = m.scope().if_scope(Expression::new_zero())?;

		scope2.new_register()?
			.clk(sig.into())
			.nreset(expr)
			.next(Expression::new_zero())
			.output(sig2.into())
			.build()?;

		Ok(())
	}
}