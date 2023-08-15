#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct ParenthesizedExpression {
	pub expression: Box<super::Expression>,
	pub location: crate::SourceSpan,
}