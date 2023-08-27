use super::Expression;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct UnaryCastExpression {
	pub type_name: super::TypeName,
	pub expression: Box<Expression>,
	pub location: crate::SourceSpan,
}