use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct UnaryOperatorExpression {
	pub expression: Box<Expression>,
	pub code: super::UnaryOpcode,
	pub location: crate::SourceSpan,
}