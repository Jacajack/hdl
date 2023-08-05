use super::signal::{SignalRef, SignalDirection};
use super::scope::{Scope};

pub struct Module {
	namespace_path: Vec<String>,
	name: String,
	main_scope: Scope,
	interface: Vec<(SignalRef, SignalDirection)>,
}

impl Module {
	pub fn new(name: String, namespace_path: Vec<String>, main_scope: Scope) -> Self {
		Self {
			namespace_path,
			name,	
			interface: Vec::new(),
			main_scope: main_scope,
		}
	}

	/// Exposes a signal to the module interface.
	pub fn expose(&mut self, signal: SignalRef, direction: SignalDirection) {
		todo!();
	}
}