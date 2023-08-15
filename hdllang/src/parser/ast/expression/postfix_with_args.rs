use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
pub struct PostfixWithArgs {
	pub expression: Box<Expression>,
	pub argument_list: Vec<Expression>,
	pub location: crate::SourceSpan,
}