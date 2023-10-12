#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
pub struct ConditionalExpression {
	pub statements: Vec<super::MatchExpressionStatement>,
	pub location: crate::SourceSpan,
}

impl ConditionalExpression {
	pub fn get_default(&self) -> Option<&super::MatchExpressionStatement> {
		for statement in &self.statements {
			if let super::super::MatchExpressionAntecendent::Default { .. } = statement.antecedent {
				return Some(statement);
			}
		}
		None
	}
}
