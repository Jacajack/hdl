use hirn::design::ScopeHandle;

use super::ModuleImplementationStatement;
use crate::analyzer::GlobalAnalyzerContext;
use crate::analyzer::LocalAnalyzerContext;
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct ModuleImplementationBlockStatement {
	pub statements: Vec<ModuleImplementationStatement>,
	pub location: SourceSpan,
}

impl ModuleImplementationBlockStatement {
	pub fn analyze(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		scope_id: usize,
	) -> miette::Result<()> {
		let new_id = local_ctx.scope.new_scope(Some(scope_id));
		local_ctx.scope_map.insert(self.location, new_id);
		for statement in &self.statements {
			statement.first_pass(ctx, local_ctx, new_id)?;
		}
		Ok(())
	}
	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		let mut subscope = api_scope.new_subscope().unwrap();
		for statement in &self.statements {
			statement.codegen_pass(ctx, local_ctx, &mut subscope)?;
		}
		Ok(())
	}
}
