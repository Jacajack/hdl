#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq, Hash)]
pub struct Number {
	pub key: crate::lexer::NumericConstantTableKey,
	pub location: crate::SourceSpan,
}
