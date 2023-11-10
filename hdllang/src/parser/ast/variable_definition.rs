mod pretty_printable;

use hirn::design::ScopeHandle;

use crate::analyzer::{AlreadyCreated, VariableKind, GlobalAnalyzerContext, LocalAnalyzerContext, SemanticError, BusWidth, GenericVariable, Variable, AdditionalContext};
use crate::lexer::CommentTableKey;
use crate::parser::ast::{DirectInitializer, SourceLocation, TypeDeclarator};
use crate::{SourceSpan, ProvidesCompilerDiagnostic};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct VariableDefinition {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub initializer_list: Vec<DirectInitializer>,
	pub location: SourceSpan,
}

impl SourceLocation for VariableDefinition {
	fn get_location(&self) -> SourceSpan {
		self.location
	}
}

impl VariableDefinition {
	pub fn analyze(
		&self,
		already_created: AlreadyCreated,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContext,
		scope_id: usize,
	) -> miette::Result<()> {
		use log::*;
		local_ctx.scope_map.insert(self.location, scope_id);
		let kind = VariableKind::from_type_declarator(
			&self.type_declarator,
			scope_id,
			already_created,
			ctx.nc_table,
			ctx.id_table,
			&mut local_ctx.scope,
		)?;
		match &kind {
			VariableKind::Signal(sig) => {
				if sig.is_direction_specified() {
					return Err(miette::Report::new(
						SemanticError::SignalDirectionSpecified
							.to_diagnostic_builder()
							.label(
								self.location,
								"This signal is specified as interface's signal in module implementation",
							)
							.build(),
					));
				}
			},
			VariableKind::Generic(_) => (),
			VariableKind::ModuleInstance(_) => (),
		}
		for direct_initializer in &self.initializer_list {
			let mut spec_kind = kind.clone();
			if let Some(variable) = local_ctx
				.scope
				.get_variable_in_scope(scope_id, &direct_initializer.declarator.name)
			{
				return Err(miette::Report::new(
					SemanticError::DuplicateVariableDeclaration
						.to_diagnostic_builder()
						.label(
							direct_initializer.declarator.get_location(),
							format!(
								"Variable with name \"{}\" declared here, was already declared before.",
								ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap()
							)
							.as_str(),
						)
						.label(
							variable.var.location,
							format!(
								"Here variable \"{}\" was declared before.",
								ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap()
							)
							.as_str(),
						)
						.build(),
				));
			}
			let mut dimensions = Vec::new();
			if spec_kind.is_generic() && direct_initializer.declarator.array_declarators.len() > 0 {
				return Err(miette::Report::new(
					SemanticError::GenericArray
						.to_diagnostic_builder()
						.label(
							*&direct_initializer
								.declarator
								.array_declarators
								.first()
								.unwrap()
								.get_location(),
							"There was an attempt to declare an array of generic variables",
						)
						.build(),
				));
			}
			for array_declarator in &direct_initializer.declarator.array_declarators {
				let size = array_declarator.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)?;
				local_ctx.scope.evaluated_expressions.insert(
					array_declarator.get_location(),
					crate::analyzer::module_implementation_scope::EvaluatedEntry::new(
						array_declarator.clone(),
						scope_id,
					),
				);
				match &size {
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
			spec_kind.add_dimenstions(dimensions);
			match &direct_initializer.expression {
				Some(expr) => {
					debug!("Assignment in initialization takes place in {:?} scope", scope_id);
					debug!("Scope is {:?}", local_ctx.scope.get_scope(scope_id));
					if spec_kind.is_array() {
						return Err(miette::Report::new(
							SemanticError::ArrayInExpression
								.to_diagnostic_builder()
								.label(
									direct_initializer.get_location(),
									"Array cannot be initialized with expression",
								)
								.build(),
						));
					}
					if spec_kind.is_generic() {
						let rhs_val = expr.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)?;
						if let VariableKind::Generic(GenericVariable { value, .. }) = &mut spec_kind {
							value.replace(BusWidth::Evaluated(rhs_val.unwrap()));
						}
						else {
							unreachable!()
						}
						let id = local_ctx.scope.define_variable(
							scope_id,
							Variable {
								name: direct_initializer.declarator.name,
								kind: spec_kind,
								location: direct_initializer.declarator.get_location(),
							},
						)?;
						let mut var = local_ctx.scope.get_variable_by_id(id).expect("It was just declared");
						let mut lhs = var.var.kind.to_signal().expect("This was checked during analysis");
						debug!("Lhs is {:?}", lhs);
						let rhs = expr.evaluate_type(
							ctx,
							scope_id,
							local_ctx,
							lhs.clone(),
							false,
							direct_initializer.declarator.get_location(),
						)?;
						debug!("Rhs is {:?}", rhs);
						if rhs.is_array() {
							return Err(miette::Report::new(
								SemanticError::ArrayInExpression
									.to_diagnostic_builder()
									.label(
										direct_initializer.get_location(),
										"Array cannot be initialized with expression",
									)
									.build(),
							));
						}
						lhs.evaluate_as_lhs(true, ctx, rhs, direct_initializer.declarator.get_location())?;
						var.var.kind = VariableKind::Signal(lhs);
						local_ctx.scope.redeclare_variable(var);
						let entries = expr.get_sensitivity_entry(ctx, local_ctx, scope_id);
						local_ctx
							.sensitivity_graph
							.add_edges(
								entries,
								crate::analyzer::SensitivityGraphEntry::Signal(id, direct_initializer.location),
								expr.get_location(),
							)
							.map_err(|e| e.build())?;
					}
					else {
						let mut lhs = spec_kind.to_signal().expect("This was checked during analysis");
						debug!("Lhs is {:?}", lhs);
						let rhs = expr.evaluate_type(
							ctx,
							scope_id,
							local_ctx,
							lhs.clone(),
							false,
							direct_initializer.declarator.get_location(),
						)?;
						debug!("Rhs is {:?}", rhs);
						if rhs.is_array() {
							return Err(miette::Report::new(
								SemanticError::ArrayInExpression
									.to_diagnostic_builder()
									.label(
										direct_initializer.get_location(),
										"Array cannot be initialized with expression",
									)
									.build(),
							));
						}
						lhs.evaluate_as_lhs(true, ctx, rhs, direct_initializer.declarator.get_location())?;
						spec_kind = VariableKind::Signal(lhs);
						let id = local_ctx.scope.define_variable(
							scope_id,
							Variable {
								name: direct_initializer.declarator.name,
								kind: spec_kind,
								location: direct_initializer.declarator.get_location(),
							},
						)?;
						let entries = expr.get_sensitivity_entry(ctx, local_ctx, scope_id);
						local_ctx
							.sensitivity_graph
							.add_edges(
								entries,
								crate::analyzer::SensitivityGraphEntry::Signal(
									id,
									direct_initializer.declarator.get_location(),
								),
								direct_initializer.location,
							)
							.map_err(|e| e.build())?;
					}
				},
				None => {
					local_ctx.scope.define_variable(
						scope_id,
						Variable {
							name: direct_initializer.declarator.name,
							kind: spec_kind,
							location: direct_initializer.declarator.get_location(),
						},
					)?;
				},
			};

			debug!(
				"Defined variable {:?} in scope {}",
				ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap(),
				scope_id
			);
			debug!("Scope is {:?}", local_ctx.scope.get_scope(scope_id));
		}
		Ok(())
	}
	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContext,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		use log::*;
		let additional_ctx = AdditionalContext::new(
			local_ctx.nc_widths.clone(),
			local_ctx.array_or_bus.clone(),
			local_ctx.casts.clone(),
		);
		let scope_id = local_ctx.scope_map.get(&self.location).unwrap().to_owned();
		for direct_initializer in &self.initializer_list {
			let variable = local_ctx
				.scope
				.get_variable_in_scope(scope_id, &direct_initializer.declarator.name)
				.unwrap();
			let api_id = variable.var.register(
				ctx.nc_table,
				ctx.id_table,
				scope_id,
				&local_ctx.scope,
				Some(&additional_ctx),
				api_scope
					.new_signal(ctx.id_table.get_by_key(&variable.var.name).unwrap().as_str())
					.unwrap(),
			)?;
			match &direct_initializer.expression {
				Some(expr) => {
					let rhs = expr.codegen(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
					)?;
					debug!("Lhs is {:?}", hirn::design::Expression::Signal(api_id.into()));
					debug!("Rhs is {:?}", rhs);
					api_scope.assign(api_id.into(), rhs).unwrap()
				},
				None => (),
			}

			local_ctx.scope.insert_api_id(variable.id, api_id)
		}
		Ok(())
	}
}
