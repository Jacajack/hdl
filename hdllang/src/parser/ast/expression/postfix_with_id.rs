use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq)]
pub struct PostfixWithId {
	pub expression: Box<Expression>,
	pub id: crate::lexer::IdTableKey,
	pub location: crate::SourceSpan,
}