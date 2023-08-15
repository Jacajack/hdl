use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct PostfixWithId {
	pub expression: Box<Expression>,
	pub id: crate::lexer::IdTableKey,
	pub location: crate::SourceSpan,
}