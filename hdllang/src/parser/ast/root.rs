use crate::parser::ast::{SourceLocation, TopDefinition};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Error, Formatter};

#[derive(Serialize, Deserialize)]
pub struct Root {
	pub definitions: Vec<TopDefinition>,
	pub location: SourceSpan,
}
impl Debug for Root {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		for i in 0..self.definitions.len() {
			writeln!(fmt, "{:?}", self.definitions[i])?;
		}
		writeln!(fmt)
	}
}
impl SourceLocation for Root {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
