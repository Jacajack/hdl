mod pretty_printable;

use crate::core::SourceSpan;
use crate::parser::ast::{opcodes::*, Expression, SourceLocation};
use serde::{Deserialize, Serialize};
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RangeExpression {
	pub lhs: Box<Expression>,
	pub rhs: Box<Expression>,
	pub code: RangeOpcode,
	pub location: SourceSpan,
}
impl SourceLocation for RangeExpression {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}
