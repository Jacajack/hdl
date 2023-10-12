#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
pub struct MatchExpression {
	pub value: Box<super::Expression>,
	pub statements: Vec<crate::parser::ast::MatchExpressionStatement>,
	pub location: crate::SourceSpan,
}

impl MatchExpression {
	pub fn get_default(&self) -> Option<&super::MatchExpressionStatement> {
		for statement in &self.statements {
			if let super::super::MatchExpressionAntecendent::Default { .. } = statement.antecedent {
				return Some(statement);
			}
		}
		None
	}
}