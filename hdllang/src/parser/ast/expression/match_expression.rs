#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
pub struct MatchExpression {
	pub value: Box<super::Expression>,
	pub statements: Vec<crate::parser::ast::MatchExpressionStatement>,
	pub location: crate::SourceSpan,
}