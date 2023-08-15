mod pretty_printable;

use crate::parser::ast::{DirectDeclarator, Expression, SourceLocation};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct DirectInitializer {
	pub declarator: DirectDeclarator,
	pub expression: Option<Expression>,
	pub location: SourceSpan,
}

impl SourceLocation for DirectInitializer {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
