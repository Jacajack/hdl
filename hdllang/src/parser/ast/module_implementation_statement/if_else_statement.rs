use hirn::design::ScopeHandle;

use crate::analyzer::{GlobalAnalyzerContext, LocalAnalyzerContext};
use crate::parser::ast::Expression;
use crate::parser::ast::ModuleImplementationStatement;
use crate::{ProvidesCompilerDiagnostic, SourceSpan};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IfElseStatement {
	pub condition: Expression,
	pub if_statement: Box<ModuleImplementationStatement>,
	pub else_statement: Option<Box<ModuleImplementationStatement>>,
	pub location: SourceSpan,
}

impl IfElseStatement {
	pub fn first_pass(
		&self,
		scope_id: usize,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContext,
	) -> miette::Result<()> {
		let condition_type = self.condition.evaluate(ctx.nc_table, scope_id, &mut local_ctx.scope)?;
		let if_scope = local_ctx.scope.new_scope(Some(scope_id));
		log::debug!("Condition is {:?}", condition_type);
		let cond = condition_type.map_or_else(|| true, |val| val.value != 0.into());
		local_ctx.add_branch(cond);
		self.if_statement.first_pass(ctx, local_ctx, if_scope)?;
		local_ctx.pop_branch();
		match &self.else_statement {
			Some(stmt) => {
				let else_scope = local_ctx.scope.new_scope(Some(scope_id));
				local_ctx.add_branch(!cond);
				stmt.first_pass(ctx, local_ctx, else_scope)?;
				local_ctx.pop_branch();
			},
			None => (),
		}
		Ok(())
	}

	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContext,
		scope_id: usize,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		use crate::core::CompilerError;
		use crate::parser::ast::ModuleImplementationStatement::*;

		let additional_ctx = crate::analyzer::AdditionalContext::new(
			local_ctx.nc_widths.clone(),
			local_ctx.array_or_bus.clone(),
			local_ctx.casts.clone(),
		);

		let condition_expr = self.condition.codegen(
			ctx.nc_table,
			ctx.id_table,
			scope_id,
			&local_ctx.scope,
			Some(&additional_ctx),
		)?;

		match self.else_statement {
			Some(ref else_stmt) => {
				let (mut if_scope, mut else_scope) = api_scope.if_else_scope(condition_expr).map_err(|err| {
					CompilerError::HirnApiError(err)
						.to_diagnostic_builder()
						.label(self.location, "Error occured here")
						.build()
				})?;

				match self.if_statement.as_ref() {
					ModuleImplementationBlockStatement(block) => {
						log::debug!("Codegen for if block");
						for statement in &block.statements {
							statement.codegen_pass(ctx, local_ctx, &mut if_scope)?;
						}
					},
					_ => unreachable!(),
				};

				match else_stmt.as_ref() {
					ModuleImplementationBlockStatement(block) => {
						log::debug!("Codegen for else block");
						for statement in &block.statements {
							statement.codegen_pass(ctx, local_ctx, &mut else_scope)?;
						}
					},
					IfElseStatement(conditional) => {
						log::debug!("Codegen for else-if block");
						conditional.codegen_pass(ctx, local_ctx, scope_id, &mut else_scope)?;
					},
					_ => unreachable!(),
				};
			},
			None => {
				let mut if_scope = api_scope.if_scope(condition_expr).map_err(|err| {
					CompilerError::HirnApiError(err)
						.to_diagnostic_builder()
						.label(self.location, "Error occured here")
						.build()
				})?;

				match self.if_statement.as_ref() {
					ModuleImplementationBlockStatement(block) => {
						log::debug!("Codegen for single if block");
						for statement in &block.statements {
							statement.codegen_pass(ctx, local_ctx, &mut if_scope)?;
						}
					},
					_ => unreachable!(),
				};
			},
		};
		Ok(())
	}
}
