use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq)]
pub struct TernaryExpression {
	pub condition: Box<Expression>,
	pub true_branch: Box<Expression>,
	pub false_branch: Box<Expression>,
	pub location: crate::SourceSpan,
}