#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct 	Number {
	pub key: crate::lexer::NumericConstantTableKey,
	pub location: crate::SourceSpan,
}