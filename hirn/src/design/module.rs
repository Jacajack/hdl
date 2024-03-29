use super::{
	DesignCore, DesignError, DesignHandle, HasComment, HasSensitivity, ModuleId, ScopeHandle, ScopeId, SignalId,
};

/// Specifies direction for signals in module interface
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SignalDirection {
	/// Input signal (from the perspective of the module)
	Input,

	/// Output signal (from the perspective of the module)
	Output,

	/// Tristate signal (direction left unspecified, actually)
	Tristate,
}

/// Represents a signal exposed to a module interface
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct InterfaceSignal {
	pub signal: SignalId,
	pub direction: SignalDirection,
}

impl InterfaceSignal {
	pub fn is_input(&self) -> bool {
		matches!(self.direction, SignalDirection::Input)
	}

	pub fn is_output(&self) -> bool {
		matches!(self.direction, SignalDirection::Output)
	}

	pub fn signal(&self) -> SignalId {
		self.signal
	}
}

/// Represents a hardware module
#[derive(Debug)]
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
	pub interface: Vec<InterfaceSignal>, // TODO set?

	/// Metadata comment
	pub comment: Option<String>,

	/// Flag indicating whether the module is an interface
	should_elaborate: bool,
}

impl Module {
	/// Creates a new module
	pub fn new(name: &str, namespace_path: Vec<String>) -> Result<Self, DesignError> {
		if !super::utils::is_name_valid(name) {
			return Err(DesignError::InvalidName);
		}

		Ok(Self {
			id: ModuleId { id: 0 },
			namespace_path,
			name: name.into(),
			main_scope: ScopeId { id: 0 },
			interface: vec![],
			comment: None,
			should_elaborate: true,
		})
	}

	/// Exposes a signal to the module interface.
	fn expose(&mut self, signal: SignalId, direction: SignalDirection) -> Result<(), DesignError> {
		// TODO assert signal in main scope
		// TODO assert exposed only once

		if matches!(direction, SignalDirection::Tristate) {
			unimplemented!("Tristate signals not yet supported");
		}

		self.interface.push(InterfaceSignal { signal, direction });

		Ok(())
	}

	fn comment(&mut self, comment: &str) {
		self.comment = Some(comment.into());
	}

	fn get_interface_signal_by_name(&self, design: &DesignCore, name: &str) -> Option<InterfaceSignal> {
		for sig in &self.interface {
			if design.get_signal(sig.signal).unwrap().name == name {
				return Some(*sig);
			}
		}

		None
	}

	fn get_interface_clocks(&self, design: &DesignCore) -> Vec<SignalId> {
		let mut clocks = vec![];
		for intf_sig in &self.interface {
			let sig = design.get_signal(intf_sig.signal).unwrap();
			if sig.is_clock() {
				clocks.push(intf_sig.signal);
			}
		}
		clocks
	}

	fn set_should_elaborate(&mut self, flag: bool) {
		self.should_elaborate = flag;
	}

	fn should_elaborate(&self) -> bool {
		self.should_elaborate
	}
}

impl HasComment for Module {
	fn get_comment(&self) -> Option<String> {
		self.comment.clone()
	}
}

/// Helper macro to get a mutable reference to the current module
/// in the ModuleHandle
macro_rules! this_module_mut {
	($self:ident) => {
		$self.design.borrow_mut().get_module_mut($self.id).unwrap()
	};
}

macro_rules! this_module {
	($self:ident) => {
		$self.design.borrow().get_module($self.id).unwrap()
	};
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
		Self { design, id }
	}

	pub fn design(&self) -> DesignHandle {
		self.design.clone()
	}

	/// Returns a handle to the module's main scope
	pub fn scope(&self) -> ScopeHandle {
		ScopeHandle::new(self.design.clone(), this_module!(self).main_scope)
	}

	/// Returns interface signal's direction and ID by name
	pub fn get_interface_signal_by_name(&self, name: &str) -> Option<InterfaceSignal> {
		this_module!(self).get_interface_signal_by_name(&self.design.borrow(), name)
	}

	/// Returns all clocks in the module's interface
	pub fn get_interface_clocks(&self) -> Vec<SignalId> {
		this_module!(self).get_interface_clocks(&self.design.borrow())
	}

	/// Exposes a signal to module's interface
	pub fn expose(&mut self, signal: SignalId, direction: SignalDirection) -> Result<(), DesignError> {
		let result = this_module_mut!(self).expose(signal, direction);
		let mut design = self.design.borrow_mut();
		design.get_signal_mut(signal).unwrap().set_direction(direction);
		result
	}

	pub fn comment(&mut self, comment: &str) {
		this_module_mut!(self).comment(comment);
	}

	pub fn set_should_elaborate(&mut self, flag: bool) {
		this_module_mut!(self).set_should_elaborate(flag);
	}

	pub fn should_elaborate(&self) -> bool {
		this_module!(self).should_elaborate()
	}

	/// Returns the ID of the scope
	pub fn id(&self) -> ModuleId {
		self.id
	}

	/// Returns name of the module
	pub fn name(&self) -> String {
		this_module!(self).name.clone()
	}

	/// Returns path to the module
	pub fn namespace_path(&self) -> Vec<String> {
		this_module!(self).namespace_path.clone()
	}

	pub fn interface(&self) -> Vec<InterfaceSignal> {
		this_module!(self).interface.clone()
	}
}

impl HasComment for ModuleHandle {
	fn get_comment(&self) -> Option<String> {
		this_module!(self).get_comment()
	}
}

impl std::fmt::Debug for ModuleHandle {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", this_module!(self))
	}
}
