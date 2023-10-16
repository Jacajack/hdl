use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq, Hash)]
pub struct UnaryOperatorExpression {
	pub expression: Box<Expression>,
	pub code: super::UnaryOpcode,
	pub location: crate::SourceSpan,
}
