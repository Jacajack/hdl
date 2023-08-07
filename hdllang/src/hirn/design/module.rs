use std::collections::HashMap;
use super::{ModuleRef};
use super::signal::{SignalRef, SignalDirection};
use super::scope::{Scope, ScopeRef};

pub struct Module {
	namespace_path: Vec<String>,
	name: String,
	main_scope: ScopeRef,
	interface: Vec<(SignalRef, SignalDirection)>,
}


impl Module {
	pub fn new(anme: String, namespace_path: Vec<String>, main_scope: ScopeRef) -> Self {
		todo!();
	}

	/// Exposes a signal to the module interface.
	pub fn expose(&mut self, signal: SignalRef, direction: SignalDirection) {
		todo!();
	}
}

