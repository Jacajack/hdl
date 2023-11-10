use hirn::design::ScopeHandle;

use crate::CompilerError;
use crate::ProvidesCompilerDiagnostic;
use crate::SourceSpan;
use crate::analyzer::BusWidth;
use crate::analyzer::GenericVariable;
use crate::analyzer::Variable;
use crate::analyzer::VariableKind;
use crate::core::IdTableKey;
use crate::core::NumericConstant;
use crate::parser::ast::RangeOpcode;
use crate::parser::ast::SourceLocation;
use super::ModuleImplementationStatement;
use super::RangeExpression;
use crate::analyzer::{GlobalAnalyzerContext, LocalAnalyzerContext};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IterationStatement {
	pub id: IdTableKey,
	pub range: RangeExpression,
	pub statement: Box<ModuleImplementationStatement>,
	pub location: SourceSpan,
}

impl IterationStatement {
	pub fn first_pass(&self, scope_id: usize, ctx: &mut GlobalAnalyzerContext,local_ctx: &mut LocalAnalyzerContext)-> miette::Result<()> {
		let id = local_ctx.scope.new_scope(Some(scope_id));
		let mut initial_val = self
			.range
			.lhs
			.evaluate(ctx.nc_table, scope_id, &mut local_ctx.scope)?
			.unwrap()
			.value;
		let mut end_val = self
			.range
			.rhs
			.evaluate(ctx.nc_table, scope_id, &mut local_ctx.scope)?
			.unwrap()
			.value;
		use crate::parser::ast::RangeOpcode::*;
		match &self.range.code {
			Colon => (),
			PlusColon => end_val += initial_val.clone(),
			ColonLessThan => end_val -= 1,
		}
		local_ctx.scope_map.insert(self.statement.get_location(), id);
		while initial_val <= end_val {
			local_ctx.scope.define_variable(
				id,
				Variable::new(
					self.id,
					self.location,
					VariableKind::Generic(GenericVariable {
						value: Some(BusWidth::Evaluated(NumericConstant::new_from_value(
							initial_val.clone(),
						))),
						is_wire: false,
						direction: crate::analyzer::Direction::None,
						width: Some(BusWidth::Evaluated(NumericConstant::new_from_value(64.into()))),
						signedness: crate::analyzer::SignalSignedness::Unsigned(self.location),
						location: self.location,
					}),
				),
			)?;
			match self.statement.as_ref() {
				ModuleImplementationStatement::ModuleImplementationBlockStatement(block) => {
					for statement in &block.statements {
						statement.first_pass(ctx, local_ctx, id)?;
					}
				},
				_ => unreachable!(),
			};
			if initial_val != end_val {
				local_ctx.scope.clear_scope(id);
			}
			initial_val += 1;
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
		let additional_ctx = crate::analyzer::AdditionalContext::new(
			local_ctx.nc_widths.clone(),
			local_ctx.array_or_bus.clone(),
			local_ctx.casts.clone(),
		);
		let lhs = self.range.lhs.codegen(
			ctx.nc_table,
			ctx.id_table,
			scope_id,
			&local_ctx.scope,
			Some(&additional_ctx),
		)?;
		let mut rhs = self.range.rhs.codegen(
			ctx.nc_table,
			ctx.id_table,
			scope_id,
			&local_ctx.scope,
			Some(&additional_ctx),
		)?;
		use RangeOpcode::*;
		match &self.range.code {
			Colon => (),
			PlusColon => {
				rhs = hirn::design::Expression::Binary(hirn::design::BinaryExpression {
					op: hirn::design::BinaryOp::Add,
					lhs: Box::new(lhs.clone()),
					rhs: Box::new(rhs),
				})
			},
			ColonLessThan => {
				rhs = hirn::design::Expression::Binary(hirn::design::BinaryExpression {
					op: hirn::design::BinaryOp::Subtract,
					lhs: Box::new(rhs),
					rhs: Box::new(hirn::design::Expression::Constant(
						hirn::design::NumericConstant::new_signed(1.into()),
					)),
				})
			},
		}
		let (mut for_scope, iterator_id) = api_scope
			.loop_scope(&ctx.id_table.get_value(&self.id), lhs, rhs)
			.map_err(|err| {
				CompilerError::HirnApiError(err)
					.to_diagnostic_builder()
					.label(self.location, "Error occured here")
					.build()
			})?;
		let id = local_ctx.scope_map.get(&self.statement.get_location()).unwrap();
		local_ctx
			.scope
			.insert_api_id(local_ctx.scope.get_variable(*id, &self.id).unwrap().id, iterator_id);
		use crate::parser::ast::ModuleImplementationStatement::*;
		match self.statement.as_ref() {
			ModuleImplementationBlockStatement(block) => {
				for statement in &block.statements {
					statement.codegen_pass(ctx, local_ctx, &mut for_scope)?;
				}
			},
			_ => unreachable!(),
		};
		Ok(())
	}
}
