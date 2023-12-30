use hirn::design::ModuleHandle;

use crate::analyzer::*;
use crate::lexer::CommentTableKey;
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
		global_ctx: &mut GlobalAnalyzerContext,
		declaration_scope: &mut Box<LocalAnalyzerContext>,
		handle: &mut ModuleHandle,
	) -> miette::Result<()> {
		already_created = analyze_qualifiers(&self.types, already_created, declaration_scope, 0, &global_ctx.id_table)?;
		for statement in &self.statements {
			statement.create_variable_declaration(
				already_created.clone(),
				global_ctx,
				declaration_scope,
				handle,
			)?;
		}
		Ok(())
	}
}
