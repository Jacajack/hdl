mod pretty_printable;


use super::SourceLocation;
use super::SourceSpan;
use crate::lexer::IdTable;
use crate::lexer::IdTableKey;
use serde::{Deserialize, Serialize};
#[derive(Serialize, Deserialize, Debug)]
pub struct ImportPath {
	pub start: Start,
	pub path: Vec<IdTableKey>,
	pub modules: Modules,
	pub location: SourceSpan,
}

impl ImportPath{
	pub fn into_paths(&self, ids: &IdTable, path_from_root: &String) -> Vec<String>{
		use Start::*;
		let mut string_path = match self.start{
    		Super { number } => {
				let mut path = format!("{}/", path_from_root);
				for _ in 0..number {
					path.push_str("../");
				}
				path
			},
    		Root => String::from(""),
    		Local => format!("{}/", path_from_root),
		};
		for id in &self.path{
			string_path.push_str(&format!("{}/", ids.get_by_key(id).unwrap()));
		}
		match &self.modules{
			Modules::All => {
				// let mut path = string_path;
				string_path.push_str("*.hirl");
				let mut paths = Vec::new();
				paths.push(string_path);
				paths
			},
			Modules::Specific { modules } => {
				let mut paths = Vec::new();
				for module in modules{
					let mut path = string_path.clone();
					path.push_str(&format!("{}.hirl", ids.get_by_key(&module).unwrap()));
					paths.push(path);
				}
				paths
			}
		}
	}
}

impl SourceLocation for ImportPath {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}

#[derive(Serialize, Deserialize, Debug)]

pub enum Start {
	Super { number: usize },
	Root,
	Local,
}
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Modules {
	All,
	Specific { modules: Vec<IdTableKey> },
}



#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_to_string(){
		let mut table = IdTable::new();
		let foo_key = table.insert_or_get("foo");
		let bar_key = table.insert_or_get("bar");
		let bar_key2 = table.insert_or_get("bar");
		let bar_key3 = table.insert_or_get("bar");

		let path = ImportPath{
			start: Start::Super { number: 1 },
			path: vec![foo_key, bar_key],
			modules: Modules::Specific { modules: vec![bar_key2] },
			location: SourceSpan::new(0, 0),
		};
		assert_eq!(path.into_paths(&table, &String::from("")), vec![String::from("/../foo/bar/bar.hirl")]);
	}
}