#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq)]
pub struct Tuple {
	pub expressions: Vec<super::Expression>,
	pub location: crate::SourceSpan,
}