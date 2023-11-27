mod pretty_printable;

use std::vec;

use hirn::design::ScopeHandle;
use log::debug;

use crate::analyzer::module_implementation_scope::InternalVariableId;
use crate::analyzer::{
	AdditionalContext, BusWidth, GlobalAnalyzerContext, LocalAnalyzerContext, ModuleImplementationScope, SemanticError,
	SensitivityGraphEntry, Signal, Variable,
};
use crate::parser::ast::{Expression, SourceLocation, VariableDeclaration};
use crate::ProvidesCompilerDiagnostic;
use crate::{lexer::IdTableKey, SourceSpan};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdWithDeclaration {
	pub id: IdTableKey,
	pub declaration: VariableDeclaration,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdWithExpression {
	pub id: IdTableKey,
	pub expression: Expression,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct OnlyId {
	pub id: IdTableKey,
	pub location: SourceSpan,
}
impl SourceLocation for PortBindStatement {
	fn get_location(&self) -> SourceSpan {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.location,
			IdWithExpression(id_with_expression) => id_with_expression.location,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.location,
		}
	}
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum PortBindStatement {
	OnlyId(OnlyId),
	IdWithExpression(IdWithExpression),
	IdWithDeclaration(IdWithDeclaration),
}

impl PortBindStatement {
	pub fn get_id(&self) -> IdTableKey {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.id,
			IdWithExpression(id_with_expression) => id_with_expression.id,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.id,
		}
	}
	pub fn location(&self) -> SourceSpan {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.location,
			IdWithExpression(id_with_expression) => id_with_expression.location,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.location,
		}
	}
	pub fn get_sensitivity_entry(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		local_ctx: &LocalAnalyzerContext,
		scope_id: usize,
	) -> Vec<SensitivityGraphEntry> {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => {
				let var = local_ctx.scope.get_variable(scope_id, &only_id.id).unwrap();
				vec![SensitivityGraphEntry::Signal(var.id, var.var.location)]
			},
			IdWithExpression(id_with_expression) => id_with_expression
				.expression
				.get_sensitivity_entry(global_ctx, local_ctx, scope_id),
			IdWithDeclaration(id_with_declaration) => {
				let var = local_ctx
					.scope
					.get_variable(
						scope_id,
						&id_with_declaration.declaration.direct_declarators.first().unwrap().name,
					)
					.unwrap()
					.clone();
				vec![SensitivityGraphEntry::Signal(var.id, var.var.location)]
			},
		}
	}
	pub fn get_internal_id(
		&self,
		scope: &ModuleImplementationScope,
		scope_id: usize,
	) -> (InternalVariableId, SourceSpan) {
		use self::PortBindStatement::*;
		match &self {
			OnlyId(only_id) => {
				let var = scope.get_variable(scope_id, &only_id.id).unwrap();
				(var.id, var.var.location)
			},
			IdWithExpression(id_with_expression) => id_with_expression.expression.get_internal_id(scope, scope_id),
			IdWithDeclaration(id_with_decl) => {
				let var = scope
					.get_variable(
						scope_id,
						&id_with_decl.declaration.direct_declarators.first().unwrap().name,
					)
					.unwrap();
				(var.id, var.var.location)
			},
		}
	}
	pub fn get_type(
		&self,
		ctx: &GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		scope_id: usize,
		interface_signal: Signal,
		is_output: bool,
	) -> miette::Result<Signal> {
		use PortBindStatement::*;
		match self {
			OnlyId(id) => {
				let mut var = local_ctx
					.scope
					.get_var(scope_id, &id.id)
					.map_err(|err| err.label(self.location(), "This variable was not declared").build())?
					.clone();
				let mut sig = var.var.kind.to_signal().map_err(|err| {
					err.label(self.location(), "This identifier cannot represent a signal")
						.build()
				})?;
				sig.evaluate_as_lhs(is_output, ctx, interface_signal, self.location())?;
				if var.var.kind == crate::analyzer::VariableKind::Signal(sig.clone()) {
					return Ok(sig);
				}
				var.var.kind = crate::analyzer::VariableKind::Signal(sig.clone());
				local_ctx.scope.redeclare_variable(var);
				Ok(sig)
			},
			IdWithExpression(expr) => {
				if is_output && !expr.expression.is_lvalue() {
					return Err(miette::Report::new(
						crate::analyzer::InstanceError::ArgumentsMismatch
							.to_diagnostic_builder()
							.label(
								expr.location,
								"This expression must be a lvalue, because it is an output",
							)
							.build(),
					));
				}
				Ok(expr.expression.evaluate_type(
					ctx,
					scope_id,
					local_ctx,
					interface_signal,
					is_output,
					self.location(),
				)?)
			},
			IdWithDeclaration(id_decl) => {
				log::debug!("Id with declaration");
				let mut new_war = crate::analyzer::VariableKind::from_type_declarator(
					&id_decl.declaration.type_declarator,
					scope_id,
					crate::analyzer::AlreadyCreated::new(),
					ctx.nc_table,
					ctx.id_table,
					&mut local_ctx.scope,
				)?;
				let direct_declarator = id_decl.declaration.direct_declarators.first().unwrap();
				let mut dimensions = Vec::new();
				for array_declarator in direct_declarator.array_declarators.iter() {
					let array_size = array_declarator.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)?;
					match &array_size {
						Some(val) => {
							if val.value <= num_bigint::BigInt::from(0) {
								return Err(miette::Report::new(
									SemanticError::NegativeBusWidth
										.to_diagnostic_builder()
										.label(array_declarator.get_location(), "Array size must be positive")
										.build(),
								));
							}
							dimensions.push(BusWidth::EvaluatedLocated(val.clone(), array_declarator.get_location()));
						},
						None => dimensions.push(BusWidth::Evaluable(array_declarator.get_location())),
					}
				}
				new_war.add_dimenstions(dimensions);
				let mut sig = new_war
					.to_signal()
					.expect("This was checked during analysis of declaration");
				sig.evaluate_as_lhs(is_output, &ctx, interface_signal, id_decl.location)?;
				new_war = crate::analyzer::VariableKind::Signal(sig.clone());
				debug!("WRITING");
				debug!("Scope: {:?}", scope_id);
				debug!(
					"Id: {:?}",
					&id_decl.declaration.direct_declarators.first().unwrap().name
				);
				local_ctx.scope.define_variable(
					scope_id,
					Variable::new(direct_declarator.name, id_decl.location, new_war),
				)?;
				Ok(sig)
			},
		}
	}
	pub fn codegen_pass(
		&self,
		ctx: &GlobalAnalyzerContext,
		local_ctx: &mut Box<LocalAnalyzerContext>,
		api_scope: &mut ScopeHandle,
		current_scope: usize,
	) -> miette::Result<hirn::design::Expression> {
		let additional_ctx = AdditionalContext::new(
			local_ctx.nc_widths.clone(),
			local_ctx.ncs_to_be_exted.clone(),
			local_ctx.array_or_bus.clone(),
			local_ctx.casts.clone(),
		);
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => {
				let id = only_id.id;
				let signal_id = local_ctx.scope.get_api_id(current_scope, &id).unwrap();
				Ok(hirn::design::Expression::Signal(signal_id.into()))
			},
			IdWithExpression(id_with_expression) => {
				let expression = id_with_expression.expression.codegen(
					ctx.nc_table,
					&ctx.id_table,
					current_scope,
					&local_ctx.scope,
					Some(&additional_ctx),
				)?;
				Ok(expression)
			},
			IdWithDeclaration(id_with_declaration) => {
				debug!("READING");
				debug!("Scope: {:?}", current_scope);
				debug!(
					"Id: {:?}",
					&id_with_declaration.declaration.direct_declarators.first().unwrap().name
				);
				let variable = local_ctx
					.scope
					.get_variable_in_scope(
						current_scope,
						&id_with_declaration.declaration.direct_declarators.first().unwrap().name,
					)
					.unwrap();
				let api_id = local_ctx.scope.get_api_id(current_scope, &variable.var.name).unwrap();
				Ok(api_id.into())
			},
		}
	}
}
