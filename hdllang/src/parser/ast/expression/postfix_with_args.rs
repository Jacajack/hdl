use super::Expression;
use crate::core::IdTableKey;
#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq, Hash)]
pub struct PostfixWithArgs {
	pub id: IdTableKey,
	pub argument_list: Vec<Expression>,
	pub location: crate::SourceSpan,
}
