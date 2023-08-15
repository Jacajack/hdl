#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct Tuple {
	pub expressions: Vec<super::Expression>,
	pub location: crate::SourceSpan,
}