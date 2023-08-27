mod pretty_printable;

use crate::parser::ast::{DirectDeclarator, Expression, SourceLocation};
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
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
