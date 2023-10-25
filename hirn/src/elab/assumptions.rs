use dyn_clone::DynClone;

use crate::design::SignalId;

use super::GenericVar;

/// Trait which must be implemented by all elaboration assumption
pub trait ElabAssumptionsBase: DynClone + core::fmt::Debug {
	fn get_indexed(&self, id: SignalId, indices: &Vec<GenericVar>) -> Option<GenericVar>;

	fn get(&self, id: SignalId) -> Option<GenericVar> {
		self.get_indexed(id, &vec![])
	}
}

dyn_clone::clone_trait_object!(ElabAssumptionsBase);

/// Elaboration assumptions for top-level module (i.e. no assumptions at all)
#[derive(Clone, Debug, Default)]
pub struct ElabToplevelAssumptions;

impl ElabAssumptionsBase for ElabToplevelAssumptions {
	fn get_indexed(&self, _id: SignalId, _indices: &Vec<GenericVar>) -> Option<GenericVar> {
		None
	}
}

pub struct ElabAssumptions;
