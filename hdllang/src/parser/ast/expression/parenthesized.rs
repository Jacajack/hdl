#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
pub struct ParenthesizedExpression {
	pub expression: Box<super::Expression>,
	pub location: crate::SourceSpan,
}