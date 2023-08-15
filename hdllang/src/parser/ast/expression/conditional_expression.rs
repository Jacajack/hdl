#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct ConditionalExpression {
	pub statements: Vec<super::MatchExpressionStatement>,
	pub location: crate::SourceSpan,
}