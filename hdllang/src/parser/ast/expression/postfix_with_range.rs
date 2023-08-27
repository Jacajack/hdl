use super::Expression;
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct PostfixWithRange {
	pub expression: Box<Expression>,
	pub range: super::RangeExpression,
	pub location: crate::SourceSpan,
}
