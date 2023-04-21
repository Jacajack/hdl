use crate::parser::ast::{SourceLocation, TopDefinition};
use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};

pub struct Root {
	pub definitions: Vec<TopDefinition>,
	pub location: SourceSpan,
}
impl Debug for Root {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		for i in 0..self.definitions.len() {
			write!(fmt, "{:?}\n", self.definitions[i])?;
		}
		write!(fmt, "\n")
	}
}
impl SourceLocation for Root {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
