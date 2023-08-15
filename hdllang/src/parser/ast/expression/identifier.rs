#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct Identifier {
	pub id: crate::lexer::IdTableKey,
	pub location: crate::SourceSpan,
}