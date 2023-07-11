mod pretty_printable;

use super::SourceLocation;
use super::SourceSpan;
use crate::lexer::IdTableKey;
use serde::{Deserialize, Serialize};
#[derive(Serialize, Deserialize, Debug)]
pub struct ImportPath {
	pub start: Start,
	pub path: Vec<IdTableKey>,
	pub modules: Modules,
	pub location: SourceSpan,
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
#[derive(Serialize, Deserialize, Debug)]
pub enum Modules {
	All,
	Specific { modules: Vec<IdTableKey> },
}
