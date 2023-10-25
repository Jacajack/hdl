use dyn_clone::DynClone;

use crate::design::SignalId;

use super::GenericVar;

pub struct ElabAssumptions {

}


pub trait ElabAssumptionsBase: DynClone + core::fmt::Debug {
	fn get_indexed(&self, id: SignalId, indices: &Vec<GenericVar>) -> Option<GenericVar>;
	
	fn get(&self, id: SignalId) -> Option<GenericVar> {
		self.get_indexed(id, &vec![])
	}
}

dyn_clone::clone_trait_object!(ElabAssumptionsBase);