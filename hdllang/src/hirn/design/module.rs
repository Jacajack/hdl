use super::{ModuleRef, signal::SignalDirection, ScopeRef, SignalRef, WeakDesignHandle};

pub struct Module {
	pub(super) design: WeakDesignHandle,
	pub(super) id: ModuleRef,
	namespace_path: Vec<String>,
	name: String,
	main_scope: ScopeRef,
	interface: Vec<(SignalRef, SignalDirection)>,
}

impl Module {
	pub fn new(anme: String, namespace_path: Vec<String>, main_scope: ScopeRef) -> Self {
		todo!();
	}

	pub(super) fn set_design(&mut self, design: WeakDesignHandle, id: usize) {
		assert!(self.id.is_null());
		self.design = design;
		self.id = ModuleRef{id};
	}

	/// Exposes a signal to the module interface.
	pub fn expose(&mut self, signal: SignalRef, direction: SignalDirection) {
		todo!();
	}
}
