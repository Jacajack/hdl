mod pretty_printable;

use crate::parser::ast::{SourceLocation, TopDefinition};
use crate::SourceSpan;


#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct Root {
	pub definitions: Vec<TopDefinition>,
	pub location: SourceSpan,
}

impl SourceLocation for Root {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
