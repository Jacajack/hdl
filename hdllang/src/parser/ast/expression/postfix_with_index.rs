#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct PostfixWithIndex {
	pub expression: Box<super::Expression>,
	pub index: Box<super::Expression>,
	pub location: crate::SourceSpan,
}