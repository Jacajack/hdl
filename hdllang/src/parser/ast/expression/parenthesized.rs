#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq, Hash)]
pub struct ParenthesizedExpression {
	pub expression: Box<super::Expression>,
	pub location: crate::SourceSpan,
}
