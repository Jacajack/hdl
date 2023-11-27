use hirn::design::ScopeHandle;

use super::AssignmentOpcode;
use super::Expression;
use crate::analyzer::AdditionalContext;
use crate::analyzer::BusWidth;
use crate::analyzer::GlobalAnalyzerContext;
use crate::analyzer::LocalAnalyzerContext;
use crate::analyzer::RegisterInstance;
use crate::analyzer::SemanticError;
use crate::analyzer::Signal;
use crate::parser::ast::SourceLocation;
use crate::CompilerError;
use crate::ProvidesCompilerDiagnostic;
use crate::SourceSpan;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct AssignmentStatement {
	pub lhs: Expression,
	pub assignment_opcode: AssignmentOpcode,
	pub rhs: Expression,
	pub location: SourceSpan,
}

impl AssignmentStatement {
	pub fn first_pass(
		&self,
		scope_id: usize,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
	) -> miette::Result<()> {
		use log::*;
		debug!("Assignment takes place in {:?} scope", scope_id);
		if !self.lhs.is_lvalue() {
			report_not_allowed_lhs(self.lhs.get_location())?;
		}
		if self.lhs.is_generic(ctx, scope_id, local_ctx)? {
			debug!("Lhs is generic");
			match self.rhs.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)? {
				Some(val) => self.lhs.assign(
					BusWidth::EvaluatedLocated(val, self.rhs.get_location()),
					local_ctx,
					scope_id,
				),
				None => self
					.lhs
					.assign(BusWidth::Evaluable(self.rhs.get_location()), local_ctx, scope_id),
			}
			.map_err(|e| e.label(self.location, "This self is invalid").build())?;
			//return Ok(());
		}
		let lhs_type = self
			.lhs
			.evaluate_type(ctx, scope_id, local_ctx, Signal::new_empty(), true, self.location)?;
		info!("Lhs type at the beginning: {:?}", lhs_type);
		if lhs_type.is_array() {
			return Err(miette::Report::new(
				SemanticError::ArrayInExpression
					.to_diagnostic_builder()
					.label(self.location, "Array statement is not supported yet")
					.build(),
			));
		}
		let rhs_type = self
			.rhs
			.evaluate_type(ctx, scope_id, local_ctx, lhs_type, true, self.location)?;
		if rhs_type.width().is_none() {
			return Err(miette::Report::new(
				SemanticError::WidthNotKnown
					.to_diagnostic_builder()
					.label(self.location, "This expression has unknown width")
					.build(),
			));
		}
		if rhs_type.get_signedness().is_none() {
			return Err(miette::Report::new(
				SemanticError::SignednessMismatch // FIXME when fixing errors msgs
					.to_diagnostic_builder()
					.label(self.location, "This expression has unknown signedness")
					.build(),
			));
		}
		if rhs_type.is_array() {
			return Err(miette::Report::new(
				SemanticError::ArrayInExpression
					.to_diagnostic_builder()
					.label(self.location, "Array self is not supported yet")
					.label(self.rhs.get_location(), "This expression is an array")
					.build(),
			));
		}
		info!("Rhs type at the end: {:?}", rhs_type);
		let new_lhs = self
			.lhs
			.evaluate_type(ctx, scope_id, local_ctx, rhs_type, true, self.location)?;
		let (left_id, loc) = self.lhs.get_internal_id(&local_ctx.scope, scope_id);
		let entries = self.rhs.get_sensitivity_entry(ctx, local_ctx, scope_id);
		debug!("Adding edges {:?} to {:?}", entries, left_id);
		local_ctx
			.sensitivity_graph
			.add_edges(
				entries,
				crate::analyzer::SensitivityGraphEntry::Signal(left_id, loc),
				self.location,
			)
			.map_err(|e| e.build())?;
		info!("Lhs type at the and: {:?}", new_lhs);
		Ok(())
	}

	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		scope_id: usize,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		use log::*;
		let additional_ctx = AdditionalContext::new(
			local_ctx.nc_widths.clone(),
			local_ctx.ncs_to_be_exted.clone(),
			local_ctx.array_or_bus.clone(),
			local_ctx.casts.clone(),
		);
		let lhs = self.lhs.codegen(
			ctx.nc_table,
			ctx.id_table,
			scope_id,
			&local_ctx.scope,
			Some(&additional_ctx),
		)?;
		let rhs = self.rhs.codegen(
			ctx.nc_table,
			ctx.id_table,
			scope_id,
			&local_ctx.scope,
			Some(&additional_ctx),
		)?;
		debug!("Codegen for assignment");
		debug!("Lhs is {:?}", lhs);
		debug!("Rhs is {:?}", rhs);
		api_scope.assign(lhs, rhs).map_err(|err| {
			CompilerError::HirnApiError(err)
				.to_diagnostic_builder()
				.label(self.location, "Error occured here")
				.build()
		})?;
		debug!("Assignment done");
		Ok(())
	}
}

fn report_not_allowed_lhs(location: SourceSpan) -> miette::Result<RegisterInstance> {
	return Err(miette::Report::new(
		crate::ProvidesCompilerDiagnostic::to_diagnostic_builder(&SemanticError::ForbiddenExpressionInLhs)
			.label(
				location,
				"This expression is not allowed in the left hand sight of assignment",
			)
			.build(),
	));
}
