use hirn::design::ModuleHandle;

use crate::analyzer::*;
use crate::lexer::CommentTableKey;
use crate::lexer::IdTable;
use crate::parser::ast::ModuleDeclarationStatement;
use crate::SourceSpan;

use super::TypeQualifier;
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct ModuleDeclarationVariableBlock {
	pub metadata: Vec<CommentTableKey>,
	pub types: Vec<TypeQualifier>,
	pub statements: Vec<ModuleDeclarationStatement>,
	pub location: SourceSpan,
}
impl ModuleDeclarationVariableBlock {
	pub fn create_variable_declaration(
		&self,
		mut already_created: AlreadyCreated,
		nc_table: &crate::core::NumericConstantTable,
		id_table: &IdTable,
		comment_table: &crate::lexer::CommentTable,
		declaration_scope: &mut Box<LocalAnalyzerContext>,
		handle: &mut ModuleHandle,
	) -> miette::Result<()> {
		already_created = analyze_qualifiers(&self.types, already_created, declaration_scope, 0, id_table)?;
		for statement in &self.statements {
			statement.create_variable_declaration(
				already_created.clone(),
				nc_table,
				comment_table,
				id_table,
				declaration_scope,
				handle,
			)?;
		}
		Ok(())
	}
}
