mod pretty_printable;

use hirn::design::ScopeHandle;
use log::debug;

use crate::analyzer::{BusWidth, GlobalAnalyzerContext, LocalAnalyzerContex, SemanticError, Signal, Variable};
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
	pub fn get_type(
		&self,
		ctx: &GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
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
					.map_err(|mut err| err.label(self.location(), "This variable was not declared").build())?
					.clone();
				let mut sig = var.var.kind.to_signal();
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
				new_war.add_name_to_clock(direct_declarator.name);
				let mut sig = new_war.to_signal();
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
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle,
		current_scope: usize,
	) -> miette::Result<hirn::design::Expression> {
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
					Some(&local_ctx.nc_widths),
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
				let api_id = variable.var.register(
					ctx.nc_table,
					ctx.id_table,
					current_scope,
					&local_ctx.scope,
					Some(&local_ctx.nc_widths),
					api_scope
						.new_signal(ctx.id_table.get_by_key(&variable.var.name).unwrap().as_str())
						.unwrap(),
				)?;
				local_ctx.scope.insert_api_id(variable.id, api_id.clone());
				Ok(api_id.into())
			},
		}
	}
}
