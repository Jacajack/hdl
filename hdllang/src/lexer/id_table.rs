use bimap::BiHashMap;

/// Opaque key type for the lexer ID table
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct IdTableKey {
	key: usize,
}

/// Lexer's ID table - used to avoid storing tokens in strings
pub struct IdTable {
	ids: BiHashMap<IdTableKey, String>,
}

impl IdTable {
	/// Creates a new ID table
	pub fn new() -> IdTable {
		IdTable {
			ids: BiHashMap::<IdTableKey, String>::new(),
		}
	}

	/// Checks if given name is known
	pub fn contains_name(&self, name: &str) -> bool { self.get_by_name(name).is_some() }

	/// Checks if key is known
	pub fn contains_key(&self, key: &IdTableKey) -> bool { self.get_by_key(key).is_some() }

	/// Gets ID string by key (or None)
	pub fn get_by_key(&self, key: &IdTableKey) -> Option<&String> { self.ids.get_by_left(&key) }

	/// Gets key by identifier name (or None)
	pub fn get_by_name(&self, name: &str) -> Option<IdTableKey> {
		self.ids.get_by_right(name).copied()
	}

	/// Inserts a new string or returns the key for it
	pub fn insert_or_get(&mut self, name: &str) -> IdTableKey {
		match self.get_by_name(name) {
			Some(id) => id,
			None => {
				let new_id = IdTableKey {
					key: self.ids.len(),
				};
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
	fn into_iter(self) -> Self::IntoIter { self.ids.into_iter() }
}
