use std::collections::HashMap;

use crate::lexer::NumericConstant;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NumericConstantTableKey {
	key: usize,
}

pub struct NumericConstantTable {
	constants: HashMap<NumericConstantTableKey, NumericConstant>,
}

impl NumericConstantTable {
	pub fn new() -> Self {
		Self {
			constants: HashMap::new(),
		}
	}

	pub fn get_by_key(&self, key: &NumericConstantTableKey) -> Option<&NumericConstant> {
		self.constants.get(&key)
	}

	pub fn insert(&mut self, n: NumericConstant) -> NumericConstantTableKey {
		let key = NumericConstantTableKey {
			key: self.constants.len(),
		};
		self.constants.insert(key, n);
		key
	}
}
