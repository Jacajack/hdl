#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
pub struct ConditionalExpression {
	pub statements: Vec<super::MatchExpressionStatement>,
	pub location: crate::SourceSpan,
}
