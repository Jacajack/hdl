use std::collections::HashMap;

use dyn_clone::DynClone;

use crate::design::{ModuleId, SignalId, ModuleHandle};

use super::{ElabError, ElabMessage, GenericVar};

pub trait ElabAssumptionsBase: DynClone + core::fmt::Debug {
	fn get_indexed(&self, id: SignalId, indices: &Vec<GenericVar>) -> Option<GenericVar>;
	
	fn get(&self, id: SignalId) -> Option<GenericVar> {
		self.get_indexed(id, &vec![])
	}
}

dyn_clone::clone_trait_object!(ElabAssumptionsBase);

pub trait Elaborator {
	fn elaborate(&mut self, id: ModuleId, assumptions: &dyn ElabAssumptionsBase) -> Result<Vec<ElabMessage>, ElabError>;

	fn elaborate_bind_interface(&mut self, _module: ModuleHandle, _bindings: &HashMap<String, GenericVar>) -> Result<Vec<ElabMessage>, ElabError> {
		todo!();
	}
}