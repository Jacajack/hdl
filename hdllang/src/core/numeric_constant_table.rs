use std::collections::HashMap;

use super::*;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;

#[derive(Serialize, Deserialize, Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NumericConstantTableKey {
	key: usize,
}

#[serde_as]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NumericConstantTable {
	#[serde_as(as = "Vec<(_, _)>")]
	constants: HashMap<NumericConstantTableKey, NumericConstant>,
}

impl NumericConstantTable {
	pub fn new() -> Self {
		Self {
			constants: HashMap::new(),
		}
	}

	pub fn get_by_key(&self, key: &NumericConstantTableKey) -> Option<&NumericConstant> {
		self.constants.get(key)
	}

	pub fn insert(&mut self, n: NumericConstant) -> NumericConstantTableKey {
		let key = NumericConstantTableKey {
			key: self.constants.len(),
		};
		self.constants.insert(key, n);
		key
	}

	/// Gets numeric constant by key. Panics if key is not present
	pub fn get_value(&self, key: &NumericConstantTableKey) -> &NumericConstant {
		self.get_by_key(key).unwrap()
	}
}
