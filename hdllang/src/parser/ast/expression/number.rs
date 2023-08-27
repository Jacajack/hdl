#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
pub struct 	Number {
	pub key: crate::lexer::NumericConstantTableKey,
	pub location: crate::SourceSpan,
}