use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct BinaryExpression {
	pub lhs: Box<Expression>,
	pub rhs: Box<Expression>,
	pub code: super::BinaryOpcode,
	pub location: crate::SourceSpan,
}