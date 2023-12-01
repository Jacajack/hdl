use std::{collections::HashMap, sync::Arc};

use dyn_clone::DynClone;

use crate::design::{DesignHandle, EvalAssumptions, NumericConstant, SignalId};

use super::GenericVar;

/// Trait which must be implemented by all elaboration assumption
pub trait ElabAssumptionsBase: DynClone + core::fmt::Debug {
	fn design(&self) -> Option<DesignHandle> {
		None
	}

	fn get_indexed(&self, id: SignalId, indices: &Vec<GenericVar>) -> Option<&NumericConstant>;

	fn get(&self, id: SignalId) -> Option<&NumericConstant> {
		self.get_indexed(id, &vec![])
	}
}

impl EvalAssumptions for &dyn ElabAssumptionsBase {
	fn design(&self) -> Option<DesignHandle> {
		(*self).design()
	}

	fn signal(&self, signal: SignalId, indices: &Vec<GenericVar>) -> Option<&NumericConstant> {
		self.get_indexed(signal, indices)
	}

	fn scalar_signal(&self, signal: SignalId) -> Option<&NumericConstant> {
		self.get(signal)
	}
}

impl EvalAssumptions for Arc<dyn ElabAssumptionsBase> {
	fn design(&self) -> Option<DesignHandle> {
		self.as_ref().design()
	}

	fn signal(&self, signal: SignalId, indices: &Vec<GenericVar>) -> Option<&NumericConstant> {
		self.get_indexed(signal, indices)
	}

	fn scalar_signal(&self, signal: SignalId) -> Option<&NumericConstant> {
		self.get(signal)
	}
}

dyn_clone::clone_trait_object!(ElabAssumptionsBase);

/// Elaboration assumptions for top-level module (i.e. no assumptions at all)
#[derive(Clone, Debug)]
pub struct ElabToplevelAssumptions {
	design: DesignHandle,
}

impl ElabToplevelAssumptions {
	pub fn new(design: DesignHandle) -> Self {
		Self { design }
	}
}

impl ElabAssumptionsBase for ElabToplevelAssumptions {
	fn design(&self) -> Option<DesignHandle> {
		Some(self.design.clone())
	}

	fn get_indexed(&self, _id: SignalId, _indices: &Vec<GenericVar>) -> Option<&NumericConstant> {
		None
	}
}

#[derive(Clone, Debug)]
pub struct ElabAssumptions {
	design: Option<DesignHandle>,
	parent: Option<Arc<dyn ElabAssumptionsBase>>,
	scalar_assumptions: HashMap<SignalId, NumericConstant>,
	assumptions: HashMap<(SignalId, Vec<GenericVar>), NumericConstant>,
}

impl ElabAssumptions {
	pub fn new(design: Option<DesignHandle>) -> Self {
		Self {
			design,
			parent: None,
			scalar_assumptions: HashMap::new(),
			assumptions: HashMap::new(),
		}
	}

	pub fn new_with_parent(parent: Arc<dyn ElabAssumptionsBase>) -> Self {
		Self {
			design: parent.design(),
			parent: Some(parent),
			scalar_assumptions: HashMap::new(),
			assumptions: HashMap::new(),
		}
	}

	pub fn assume_indexed(&mut self, id: SignalId, indices: &Vec<GenericVar>, val: NumericConstant) {
		self.assumptions.insert((id, indices.clone()), val);
	}

	pub fn assume(&mut self, id: SignalId, val: NumericConstant) {
		self.scalar_assumptions.insert(id, val);
	}
}

impl ElabAssumptionsBase for ElabAssumptions {
	fn design(&self) -> Option<DesignHandle> {
		if let Some(ref design) = self.design {
			Some(design.clone())
		}
		else if let Some(ref parent) = self.parent {
			parent.design()
		}
		else {
			None
		}
	}

	fn get_indexed(&self, id: SignalId, indices: &Vec<GenericVar>) -> Option<&NumericConstant> {
		if indices.is_empty() {
			return self.get(id);
		}

		match self.assumptions.get(&(id, indices.clone())) {
			Some(val) => Some(val),
			None => match self.parent {
				Some(ref p) => p.get_indexed(id, indices),
				None => None,
			},
		}
	}

	fn get(&self, id: SignalId) -> Option<&NumericConstant> {
		match self.scalar_assumptions.get(&id) {
			Some(val) => Some(val),
			None => match self.parent {
				Some(ref p) => p.get(id),
				None => None,
			},
		}
	}
}
