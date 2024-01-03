mod pretty_printable;

use hirn::design::ScopeHandle;

use crate::analyzer::{AlreadyCreated, GlobalAnalyzerContext, LocalAnalyzerContext};
use crate::lexer::CommentTableKey;
use crate::parser::ast::{SourceLocation, TypeQualifier, VariableDefinition};
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum VariableBlockStatement {
	VariableDefinition(VariableDefinition),
	VariableBlock(VariableBlock),
}
impl SourceLocation for VariableBlockStatement {
	fn get_location(&self) -> SourceSpan {
		use self::VariableBlockStatement::*;
		match self {
			VariableDefinition(definition) => definition.location,
			VariableBlock(block) => block.location,
		}
	}
}

impl VariableBlockStatement {
	pub fn analyze(
		&self,
		already_created: AlreadyCreated,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		scope_id: usize,
	) -> miette::Result<()> {
		match self {
			VariableBlockStatement::VariableBlock(block) => {
				block.analyze(ctx, local_ctx, already_created, scope_id)?;
			},
			VariableBlockStatement::VariableDefinition(definition) => {
				definition.analyze(already_created, ctx, local_ctx, scope_id)?;
			},
		}
		Ok(())
	}
	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		match self {
			VariableBlockStatement::VariableBlock(block) => {
				block.codegen_pass(ctx, local_ctx, api_scope)?;
			},
			VariableBlockStatement::VariableDefinition(definition) => {
				definition.codegen_passv2(ctx, local_ctx, api_scope)?;
			},
		}
		Ok(())
	}
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct VariableBlock {
	pub metadata: Vec<CommentTableKey>,
	pub types: Vec<TypeQualifier>,
	pub statements: Vec<VariableBlockStatement>,
	pub location: SourceSpan,
}

impl SourceLocation for VariableBlock {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}

impl VariableBlock {
	pub fn analyze(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		mut already_created: AlreadyCreated,
		scope_id: usize,
	) -> miette::Result<()> {
		already_created =
			crate::analyzer::analyze_qualifiers(&self.types, already_created, local_ctx, scope_id, &ctx.id_table)?;
		for statement in &self.statements {
			statement.analyze(already_created.clone(), ctx, local_ctx, scope_id)?;
		}
		Ok(())
	}
	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		for statement in &self.statements {
			statement.codegen_pass(ctx, local_ctx, api_scope)?;
		}
		Ok(())
	}
}
