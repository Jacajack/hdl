use std::collections::HashMap;

/// Used to access metadata comments
use serde::{Deserialize, Serialize};
use serde_with::serde_as;

#[derive(Serialize, Deserialize, Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct CommentTableKey {
	key: usize,
}

/// Stores metadata comments
#[serde_as]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CommentTable {
	#[serde_as(as = "Vec<(_, _)>")]
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
		self.comments.get(key)
	}

	/// Stores a new metadata comment
	pub fn insert(&mut self, s: String) -> CommentTableKey {
		let key = CommentTableKey {
			key: self.comments.len(),
		};
		self.comments.insert(key, s);
		key
	}

	/// Gets comment string by key. Panics if key is not present
	pub fn get_value(&self, key: &CommentTableKey) -> &String {
		self.get_by_key(key).unwrap()
	}
}
