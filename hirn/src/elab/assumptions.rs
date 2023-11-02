use std::{rc::Rc, collections::HashMap, sync::Arc};

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

#[derive(Clone, Debug)]
pub struct ElabAssumptions {
	parent: Option<Arc<dyn ElabAssumptionsBase>>,
	scalar_assumptions: HashMap<SignalId, GenericVar>,
	assumptions: HashMap<(SignalId, Vec<GenericVar>), GenericVar>,
}

impl ElabAssumptions {
	pub fn new() -> Self {
		Self {
			parent: None,
			scalar_assumptions: HashMap::new(),
			assumptions: HashMap::new(),
		}
	}

	pub fn new_with_parent(parent: Arc<dyn ElabAssumptionsBase>) -> Self {
		Self {
			parent: Some(parent),
			scalar_assumptions: HashMap::new(),
			assumptions: HashMap::new(),
		}
	}

	pub fn assume_indexed(&mut self, id: SignalId, indices: &Vec<GenericVar>, val: GenericVar) {
		self.assumptions.insert((id, indices.clone()), val);
	}

	pub fn assume(&mut self, id: SignalId, val: GenericVar) {
		self.scalar_assumptions.insert(id, val);
	}
}

impl ElabAssumptionsBase for ElabAssumptions {
	fn get_indexed(&self, id: SignalId, indices: &Vec<GenericVar>) -> Option<GenericVar> {
		if indices.is_empty() {
			return self.get(id);
		}

		match self.assumptions.get(&(id, indices.clone())) {
			Some(val) => Some(*val),
			None => match self.parent {
				Some(ref p) => p.get_indexed(id, indices),
				None => None,
			}
		}
	}

	fn get(&self, id: SignalId) -> Option<GenericVar> {
		match self.scalar_assumptions.get(&id) {
			Some(val) => Some(*val),
			None => match self.parent {
				Some(ref p) => p.get(id),
				None => None,
			}
		}
	}
}




