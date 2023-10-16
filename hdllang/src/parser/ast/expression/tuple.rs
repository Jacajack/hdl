#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq, Hash)]
pub struct Tuple {
	pub expressions: Vec<super::Expression>,
	pub location: crate::SourceSpan,
}
