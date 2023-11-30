use hirn::design::HasComment;

use crate::analyzer::{AdditionalContext, GlobalAnalyzerContext, LocalAnalyzerContext};
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
		let mut handle = ctx
			.modules_declared
			.get_mut(&local_ctx.module_id())
			.unwrap()
			.handle
			.clone();
		let mut api_scope = handle.scope();
		if !self.metadata.is_empty() {
			let mut comment = handle.get_comment().unwrap_or(String::new());
			log::debug!("Declaration comment: {} END", comment);
			for com in self.metadata.iter() {
				log::debug!("Read comment: {} END", ctx.comment_table.get_by_key(com).unwrap());
				comment.push_str(ctx.comment_table.get_by_key(com).unwrap());
				log::debug!("Comment after append: {}", comment);
			}
			log::debug!("Whole comment: {}", comment);
			handle.comment(comment.as_str());
		}
		use crate::parser::ast::ModuleImplementationStatement::*;
		match &self.statement {
			ModuleImplementationBlockStatement(block) => {
				let additional_ctx = AdditionalContext::new(
					local_ctx.nc_widths.clone(),
					local_ctx.ncs_to_be_exted.clone(),
					local_ctx.array_or_bus.clone(),
					local_ctx.casts.clone(),
				);
				local_ctx.scope.register_all_variables_in_scope(
					&local_ctx.depenency_graph,
					ctx.nc_table,
					ctx.id_table,
					ctx.comment_table,
					Some(&additional_ctx),
					0,
					&mut api_scope,
				);
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
