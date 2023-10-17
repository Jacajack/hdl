#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq, Hash)]
pub struct PostfixWithId {
	pub expression: crate::lexer::IdTableKey,
	pub id: crate::lexer::IdTableKey,
	pub location: crate::SourceSpan,
}
