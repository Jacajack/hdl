#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
pub struct Identifier {
	pub id: crate::lexer::IdTableKey,
	pub location: crate::SourceSpan,
}
