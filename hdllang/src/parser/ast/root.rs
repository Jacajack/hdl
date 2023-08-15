mod pretty_printable;

use crate::parser::ast::{SourceLocation, TopDefinition};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Root {
	pub definitions: Vec<TopDefinition>,
	pub location: SourceSpan,
}

impl SourceLocation for Root {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
