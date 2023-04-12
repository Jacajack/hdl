use std::collections::HashMap;

/// Used to access metadata comments
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct CommentTableKey {
	key: usize,
}


/// Stores metadata comments
pub struct CommentTable {
	comments: HashMap<CommentTableKey, String>,
}

impl CommentTable {

	/// Creates a new metadata comment table
	pub fn new() -> Self {
		Self {
			comments: HashMap::new(),
		}
	}

	/// Returns comment contents by key
	pub fn get_by_key(&self, key: &CommentTableKey) -> Option<&String> {
		self.comments.get(&key)
	}

	/// Stores a new metadata comment
	pub fn insert(&mut self, s: String) -> CommentTableKey {
		let key = CommentTableKey{key: self.comments.len()};
		self.comments.insert(key, s);
		key
	}
}


