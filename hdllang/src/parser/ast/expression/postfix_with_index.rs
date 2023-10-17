#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq, Hash)]
pub struct PostfixWithIndex {
	pub expression: Box<super::Expression>,
	pub index: Box<super::Expression>,
	pub location: crate::SourceSpan,
}
