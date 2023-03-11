use bimap::BiHashMap;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct IdTableKey {
	key: usize,
}

pub struct IdTable {
	ids : BiHashMap<IdTableKey, String>,
}

impl IdTable {
	pub fn new() -> IdTable {
		IdTable {
			ids: BiHashMap::<IdTableKey, String>::new(),
		}
	}

	pub fn contains_name(&self, name: &str) -> bool {
		self.get_by_name(name).is_some()
	}

	pub fn contains_id(&self, id: &IdTableKey) -> bool {
		self.get_by_id(id).is_some()
	}

	pub fn get_by_id(&self, id: &IdTableKey) -> Option<&String> {
		self.ids.get_by_left(&id)
	}

	pub fn get_by_name(&self, name: &str) -> Option<IdTableKey> {
		self.ids.get_by_right(name).copied()
	}

	pub fn insert_or_get(&mut self, name: &str) -> IdTableKey {
		match self.get_by_name(name) {
			Some(id) => id,
			None => {
				let new_id = IdTableKey{key: self.ids.len()};
				match self.ids.insert(new_id, String::from(name)) {
					bimap::Overwritten::Neither => new_id,
					_ => panic!("Lexer IdTableKey integrity loss!"),
				}
			}
		}
	}

}

impl IntoIterator for IdTable {
	type Item = <BiHashMap<IdTableKey, String> as IntoIterator>::Item;
	type IntoIter = <BiHashMap<IdTableKey, String> as IntoIterator>::IntoIter;
	fn into_iter(self) -> Self::IntoIter {
		self.ids.into_iter()
	}
}