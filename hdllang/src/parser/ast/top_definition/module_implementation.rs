use crate::analyzer::{GlobalAnalyzerContext, LocalAnalyzerContext};
use crate::core::{CommentTableKey, IdTableKey, SourceSpan};
use crate::parser::ast::ModuleImplementationStatement;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct ModuleImplementation {
	pub metadata: Vec<CommentTableKey>,
	pub id: IdTableKey,
	pub statement: ModuleImplementationStatement,
	pub location: SourceSpan,
}

impl ModuleImplementation {
	// Performs first pass on module implementation
	pub fn first_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
	) -> miette::Result<()> {
		use log::*;

		debug!(
			"Analyzing module implementation {}",
			ctx.id_table.get_by_key(&self.id).unwrap()
		);

		// This has to be done this way to avoid creating a inner scope with the first block
		let id = 0;
		use crate::parser::ast::ModuleImplementationStatement::*;
		match &self.statement {
			ModuleImplementationBlockStatement(block) => {
				for statement in &block.statements {
					statement.first_pass(ctx, local_ctx, id)?;
				}
			},
			_ => unreachable!(),
		};
		debug!(
			"Done analyzing module implementation {}",
			ctx.id_table.get_by_key(&self.id).unwrap()
		);
		Ok(())
	}

	pub fn second_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
	) -> miette::Result<()> {
		local_ctx.second_pass(ctx)?;
		Ok(())
	}

	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
	) -> miette::Result<()> {
		use log::*;
		debug!(
			"Codegen pass for module implementation {}",
			ctx.id_table.get_by_key(&self.id).unwrap()
		);

		let mut api_scope = ctx
			.modules_declared
			.get_mut(&local_ctx.module_id)
			.unwrap()
			.handle
			.scope();

		use crate::parser::ast::ModuleImplementationStatement::*;
		match &self.statement {
			ModuleImplementationBlockStatement(block) => {
				for statement in &block.statements {
					statement.codegen_pass(ctx, local_ctx, &mut api_scope)?;
				}
			},
			_ => unreachable!(),
		};
		debug!(
			"Done codegen pass for module implementation {}",
			ctx.id_table.get_by_key(&self.id).unwrap()
		);
		Ok(())
	}
}
