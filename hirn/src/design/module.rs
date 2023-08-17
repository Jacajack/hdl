use super::{ModuleId, signal::SignalDirection, ScopeId, SignalId, DesignHandle, ScopeHandle, DesignError, Design};

/// Represents a signal exposed to a module interface
pub struct InterfaceSignal {
	pub signal: SignalId,
	pub direction: SignalDirection,
}

/// Represents a hardware module
pub struct Module {
	/// Self-reference
	pub(super) id: ModuleId,

	/// Path to the module
	pub namespace_path: Vec<String>,

	/// Name of the module
	pub name: String,

	/// Main scope of the module
	pub main_scope: ScopeId,

	/// Signals exposed to the module interface
	pub interface: Vec<InterfaceSignal>,
}

impl Module {
	/// Creates a new module
	pub fn new(name: &str, namespace_path: Vec<String>) -> Result<Self, DesignError> {
		if !super::utils::is_name_valid(name) {
			return Err(DesignError::InvalidName);
		}
		
		Ok(Self {
			id: ModuleId{id: 0},
			namespace_path,
			name: name.into(),
			main_scope: ScopeId{id: 0},
			interface: vec![],
		})
	}

	/// Exposes a signal to the module interface.
	fn expose(&mut self, _signal: SignalId, _direction: SignalDirection) -> Result<usize, DesignError> {
		// TODO assert signal in main scope
		todo!();
	}

	fn get_interface_signal_by_name(&self, design: &Design, name: &str) -> Option<&InterfaceSignal> {
		for sig in &self.interface {
			if design.borrow().get_signal(sig.signal).unwrap().name == name {
				return Some(sig);
			}
		}

		None
	}
}

/// Helper macro to get a mutable reference to the current module
/// in the ModuleHandle
macro_rules! this_module {
	($self:ident) => {
		$self.design.borrow_mut().get_module_mut($self.id).unwrap()
	}
}

/// References a module in the design
#[derive(Clone)]
pub struct ModuleHandle {
	/// Handle to the design
	design: DesignHandle,

	/// ID of the module
	id: ModuleId,
}

impl ModuleHandle {
	/// Creates a new module handle
	pub(super) fn new(design: DesignHandle, id: ModuleId) -> Self {
		Self {
			design,
			id,
		}
	}

	/// Returns a handle to the module's main scope
	pub fn scope(&mut self) -> ScopeHandle {
		ScopeHandle::new(self.design.clone(), this_module!(self).main_scope)
	}

	/// Returns the ID of the scope
	pub fn id(&self) -> ModuleId {
		self.id
	}
}