use super::{ModuleRef, signal::SignalDirection, ScopeRef, SignalRef, WeakDesignHandle, DesignHandle, ScopeHandle};

pub struct Module {
	pub(super) id: ModuleRef,
	namespace_path: Vec<String>,
	name: String,
	main_scope: ScopeRef,
	interface: Vec<(SignalRef, SignalDirection)>,
}

impl Module {
	pub fn new(name: String, namespace_path: Vec<String>, main_scope: ScopeRef) -> Self {
		// FIXME
		Self {
			id: ModuleRef{id: 0},
			namespace_path,
			name,
			main_scope,
			interface: vec![],
		}
	}

	/// Exposes a signal to the module interface.
	pub fn expose(&mut self, signal: SignalRef, direction: SignalDirection) {
		todo!();
	}
}


macro_rules! this_module {
	($self:ident) => {
		$self.design.borrow_mut().get_module_mut($self.id).unwrap()
	}
}

pub struct ModuleHandle {
	design: DesignHandle,
	id: ModuleRef,
}

impl ModuleHandle {
	pub(super) fn new(design: DesignHandle, id: ModuleRef) -> Self {
		Self {
			design,
			id,
		}
	}

	pub fn scope(&mut self) -> ScopeHandle {
		ScopeHandle::new(self.design.clone(), this_module!(self).main_scope)
	}
}