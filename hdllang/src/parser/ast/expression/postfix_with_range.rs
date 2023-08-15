use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct PostfixWithRange {
	pub expression: Box<Expression>,
	pub range: super::RangeExpression,
	pub location: crate::SourceSpan,
}