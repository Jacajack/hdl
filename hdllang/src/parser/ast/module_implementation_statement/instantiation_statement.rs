use std::collections::HashMap;

use hirn::design::ScopeHandle;

use super::ImportPath;
use super::PortBindStatement;
use crate::analyzer::module_implementation_scope::EvaluatedEntry;
use crate::analyzer::{GlobalAnalyzerContext, LocalAnalyzerContext};
use crate::SourceSpan;
use crate::parser::ast::SourceLocation;
use crate::{
	analyzer::{
		module_implementation_scope::InternalVariableId, AdditionalContext, BusWidth, ClockSensitivityList,
		InstanceError, ModuleInstance, ModuleInstanceKind, NonRegister, RegisterInstance, SemanticError,
		SensitivityGraphEntry, Signal, SignalSensitivity, Variable, VariableKind,
	},
	lexer::{CommentTableKey, IdTableKey},
	CompilerError, ProvidesCompilerDiagnostic,
};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstantiationStatement {
	pub metadata: Vec<CommentTableKey>,
	pub module_name: ImportPath,
	pub instance_name: IdTableKey,
	pub port_bind: Vec<PortBindStatement>,
	pub location: SourceSpan,
}

impl InstantiationStatement {
	pub fn first_pass(
		&self,
		scope_id: usize,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContext,
	) -> miette::Result<()> {
		use log::*;
		let name = self.module_name.get_last_module();
		match local_ctx.scope.is_declared(scope_id, &self.instance_name) {
			Some(location) => {
				return Err(miette::Report::new(
					SemanticError::DuplicateVariableDeclaration
						.to_diagnostic_builder()
						.label(
							self.location,
							format!(
								"Variable \"{}\" is already declared in this scope",
								ctx.id_table.get_by_key(&self.instance_name).unwrap()
							)
							.as_str(),
						)
						.label(location, "Previous declaration")
						.build(),
				))
			},
			None => (),
		}
		if ctx.id_table.get_value(&name).as_str() == "reg" {
			let v = Variable::new(
				self.instance_name,
				self.location,
				VariableKind::ModuleInstance(ModuleInstance {
					module_name: self.instance_name,
					location: self.location,
					kind: crate::analyzer::ModuleInstanceKind::Register(create_register(
						self, scope_id, ctx, local_ctx,
					)?),
				}),
			);
			local_ctx.scope.define_variable(scope_id, v)?;
			return Ok(());
		}
		ctx.modules_declared
			.get_mut(&local_ctx.module_id)
			.unwrap()
			.instantiates
			.push(name.clone());

		if !ctx.modules_declared.contains_key(&name) {
			return Err(miette::Report::new(
				SemanticError::ModuleNotDeclared
					.to_diagnostic_builder()
					.label(
						self.module_name.location,
						format!("Module \"{}\" is not declared", ctx.id_table.get_by_key(&name).unwrap()).as_str(),
					)
					.build(),
			));
		}
		let instance_str = ctx.id_table.get_by_key(&self.instance_name).unwrap().clone();
		let module = ctx.modules_declared.get(&name).unwrap();
		let mut scope = module.scope.clone();
		let mut module_instance = NonRegister::new();
		let mut clock_mapping: HashMap<InternalVariableId, InternalVariableId> = HashMap::new();
		if scope.get_interface_len() != self.port_bind.len() {
			return Err(miette::Report::new(
				InstanceError::ArgumentsMismatch
					.to_diagnostic_builder()
					.label(
						self.location,
						"This binding list does not match interface of module instantiated",
					)
					.help(
						format!(
							"Interface of the module xx is {}",
							scope.display_interface(ctx.id_table)
						)
						.as_str(),
					)
					.build(),
			));
		}
		let mut expressions_to_translate: HashMap<IdTableKey, crate::parser::ast::Expression> = HashMap::new();
		debug!("Binding generic variables!");
		for stmt in &self.port_bind {
			let mut interface_variable = scope
				.get_var(0, &stmt.get_id())
				.map_err(|err| {
					err.label(
						stmt.location(),
						format!(
							"Variable \"{}\" is not declared in in interface in module \"{}\"",
							ctx.id_table.get_by_key(&stmt.get_id()).unwrap(),
							ctx.id_table.get_by_key(&name).unwrap()
						)
						.as_str(),
					)
					.build()
				})?
				.clone();
			if !interface_variable.var.kind.is_generic() {
				continue;
			}
			let sig_name = ctx.id_table.get_value(&stmt.get_id()).clone();
			let new_name = ctx
				.id_table
				.insert_or_get(format!("{instance_str}_{sig_name}_generic").as_str());
			use crate::parser::ast::PortBindStatement::*;
			match &stmt {
				OnlyId(id) => {
					debug!("Only id");
					let mut local_sig = local_ctx
						.scope
						.get_var(scope_id, &id.id)
						.map_err(|err| {
							err.label(
								id.location,
								format!(
									"Variable \"{}\" is not declared",
									ctx.id_table.get_by_key(&id.id).unwrap()
								)
								.as_str(),
							)
							.build()
						})?
						.clone();
					debug!("Local sig is {:?}", local_sig.var.kind);
					if local_sig.var.kind.is_module_instance() {
						return Err(miette::Report::new(
							InstanceError::ArgumentsMismatch
								.to_diagnostic_builder()
								.label(
									stmt.location(),
									"Here was an attempt to bind module instance as a interface signal",
								)
								.label(local_sig.var.location, "Local signal defined here")
								.build(),
						));
					}
					debug!("Remote sig is {:?}", interface_variable.var.kind);
					use VariableKind::*;
					match (&mut local_sig.var.kind, &mut interface_variable.var.kind) {
						(Signal(_), Generic(_)) => {
							return Err(miette::Report::new(
								InstanceError::ArgumentsMismatch
									.to_diagnostic_builder()
									.label(
										stmt.location(),
										"Here was an attempt to bind generic and signal togther",
									)
									.label(local_sig.var.location, "Local signal defined here")
									.build(),
							))
						},
						(Generic(gen1), Generic(gen2)) => {
							if gen1.value.is_none()  && !gen1.direction.is_input(){
								return Err(miette::Report::new(
									InstanceError::GenericArgumentWithoutValue
										.to_diagnostic_builder()
										.label(local_sig.var.location, "This variable does not have a value")
										.label(
											stmt.location(),
											"Here there was an attempt to bind generic variable without a value",
										)
										.build(),
								));
							}
							gen2.value = gen1.value.clone();
							let new_var = Variable::new(new_name, stmt.location(), interface_variable.var.kind.clone());
							scope.redeclare_variable(interface_variable);
							let id = local_ctx.scope.define_intermidiate_signal(new_var)?;
							module_instance
								.add_variable(stmt.get_id(), id)
								.map_err(|err| err.label(stmt.location(), "Variable declared here").build())?;
						},
						(..) => unreachable!(),
					}
				},
				IdWithExpression(id_expr) => {
					debug!("Id with expression");
					expressions_to_translate.insert(id_expr.id, id_expr.expression.clone());
					let new_sig = id_expr.expression.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)?;
					use VariableKind::*;
					match &mut interface_variable.var.kind {
						Generic(gen) => {
							gen.value = match new_sig{
								Some(sig) => Some(BusWidth::Evaluated(sig)),
								None => {
									let entry = EvaluatedEntry{
        							    expression: id_expr.expression.clone(),
        							    scope_id,
        							};
									local_ctx.scope.evaluated_expressions.insert(id_expr.expression.get_location(), entry);
									Some(BusWidth::Evaluable(id_expr.expression.get_location()))
								}
							}
						},
						_ => unreachable!(),
					}
					let new_var = Variable::new(new_name, stmt.location(), interface_variable.var.kind.clone());
					scope.redeclare_variable(interface_variable);
					let id = local_ctx.scope.define_intermidiate_signal(new_var)?;
					module_instance
						.add_variable(stmt.get_id(), id)
						.map_err(|err| err.label(stmt.location(), "Variable declared here").build())?;
				},
				IdWithDeclaration(_) => {
					debug!("Id with declaration");
					return Err(miette::Report::new(
						InstanceError::ArgumentsMismatch
							.to_diagnostic_builder()
							.label(
								stmt.location(),
								"Here was an attempt to bind generic and signal togther",
							)
							.build(),
					));
				},
			}
		}
		debug!("Scope is {:?}", scope);
		let f: &dyn Fn(&mut crate::parser::ast::Expression) -> Result<(), ()> = &|expr| {
			use crate::parser::ast::Expression::*;
			if let Identifier(id) = expr {
				match expressions_to_translate.get(&id.id){
    			    Some(expr2) => {
						expr.clone_from(expr2);
					},
    			    None => (),
    			}
			}
			Ok(())
		};
		for entry in scope.evaluated_expressions.values_mut(){
			let prev_loc = entry.expression.get_location();
			entry.expression.transform(f).unwrap();
			local_ctx.scope.evaluated_expressions.insert(prev_loc, entry.clone());
			log::debug!("Inserted entry at {:?}", prev_loc);
		}
		debug!("Binding clocks!");
		for stmt in &self.port_bind {
			let sig_name = ctx.id_table.get_value(&stmt.get_id()).clone();
			let new_name_str = format!("{instance_str}_{sig_name}_clk");
			let new_name = ctx.id_table.insert_or_get(new_name_str.as_str());
			let mut interface_variable = scope.get_variable(0, &stmt.get_id()).expect("This was checked").clone();
			if !interface_variable.var.is_clock() || interface_variable.var.kind.is_generic() {
				continue;
			}
			debug!("Interface variable is {:?}", interface_variable.var.kind);
			interface_variable
				.var
				.kind
				.evaluate_bus_width(&scope, &ctx.id_table, ctx.nc_table)?;
			scope.redeclare_variable(interface_variable.clone());
			debug!("Interface variable is {:?}", interface_variable.var.kind);
			let clk_type = interface_variable
				.var
				.kind
				.to_signal()
				.expect("This was checked during analysis of a module");
			let is_output = match clk_type.direction {
				crate::analyzer::Direction::Input(_) => false,
				crate::analyzer::Direction::Output(_) => true,
				_ => unreachable!(),
			};
			let mut coming = stmt.get_type(ctx, local_ctx, scope_id, clk_type.clone(), is_output)?;
			if let SignalSensitivity::Clock(..) = coming.sensitivity {
				coming.evaluate_as_lhs(false, ctx, clk_type.clone(), stmt.location())?;
				debug!("Adding variable {:?}", new_name_str);
				let new_id = local_ctx.scope.define_intermidiate_signal(Variable::new(
					new_name,
					stmt.location(),
					interface_variable.var.kind,
				))?;
				debug!(
					"Inserting clock mapping {:?} -> {:?}",
					clk_type.get_clock_name(),
					new_id
				);
				clock_mapping.insert(clk_type.get_clock_name(), new_id);
				if is_output {
					let (id, loc) = stmt.get_internal_id(&local_ctx.scope, scope_id);
					local_ctx
						.sensitivity_graph
						.add_edges(
							vec![SensitivityGraphEntry::Signal(new_id, stmt.location())],
							SensitivityGraphEntry::Signal(id, loc),
							stmt.location(),
						)
						.map_err(|e| e.build())?;
				}
				else {
					let entries = stmt.get_sensitivity_entry(ctx, local_ctx, scope_id);
					local_ctx
						.sensitivity_graph
						.add_edges(
							entries,
							SensitivityGraphEntry::Signal(new_id, stmt.location()),
							stmt.location(),
						)
						.map_err(|e| e.build())?;
				}
				module_instance
					.add_clock(stmt.get_id(), new_id)
					.map_err(|err| err.label(stmt.location(), "Variable declared here").build())?;
			}
			else {
				return Err(miette::Report::new(
					InstanceError::ArgumentsMismatch
						.to_diagnostic_builder()
						.label(stmt.location(), "Clk signal must be marked as clock")
						.build(),
				));
			}
		}
		debug!("Binding non generic variables!");
		for stmt in &self.port_bind {
			let sig_name = ctx.id_table.get_value(&stmt.get_id()).clone();
			let new_name_str = format!("{instance_str}_{sig_name}");
			let new_name = ctx.id_table.insert_or_get(new_name_str.as_str());
			let mut interface_variable = scope.get_variable(0, &stmt.get_id()).expect("This was checked").clone();
			if interface_variable.var.is_clock() || interface_variable.var.kind.is_generic() {
				continue;
			}
			debug!("Interface variable is {:?}", interface_variable.var.kind);
			interface_variable
				.var
				.kind
				.evaluate_bus_width(&scope, &ctx.id_table, ctx.nc_table)?;
			scope.redeclare_variable(interface_variable.clone());
			// translate clocks
			let mut interface_signal = interface_variable
				.var
				.kind
				.to_signal()
				.expect("This was checked during analysis of a module");
			interface_signal.translate_clocks(&clock_mapping);
			debug!("Interface signal is {:?}", interface_signal);
			let is_output = match interface_signal.direction {
				crate::analyzer::Direction::Input(_) => false,
				crate::analyzer::Direction::Output(_) => true,
				_ => unreachable!(),
			};
			let _ = stmt.get_type(ctx, local_ctx, scope_id, interface_signal.clone(), is_output)?;
			let new_id = local_ctx.scope.define_intermidiate_signal(Variable::new(
				new_name,
				stmt.location(),
				VariableKind::Signal(interface_signal),
			))?;
			if is_output {
				let (id, loc) = stmt.get_internal_id(&local_ctx.scope, scope_id);
				local_ctx
					.sensitivity_graph
					.add_edges(
						vec![SensitivityGraphEntry::Signal(new_id, stmt.location())],
						SensitivityGraphEntry::Signal(id, loc),
						stmt.location(),
					)
					.map_err(|e| e.build())?;
			}
			else {
				let entries = stmt.get_sensitivity_entry(ctx, local_ctx, scope_id);
				local_ctx
					.sensitivity_graph
					.add_edges(
						entries,
						SensitivityGraphEntry::Signal(new_id, stmt.location()),
						stmt.location(),
					)
					.map_err(|e| e.build())?;
			}
			module_instance
				.add_variable(stmt.get_id(), new_id)
				.map_err(|err| err.label(stmt.location(), "Variable declared here").build())?;
		}
		let mut recursive_calls = 0;
		if name == local_ctx.module_id {
			if local_ctx.always_true_branch(){
				return Err(miette::Report::new(
					SemanticError::RecursiveModuleInstantiation
						.to_diagnostic_builder()
						.label(
							self.module_name.location,
							format!(
								"Module \"{}\" is instantiated recursively without stop condition",
								ctx.id_table.get_by_key(&name).unwrap()
							)
							.as_str(),
						)
						.build(),
				));
			}
			local_ctx.number_of_recursive_calls += 1;
			recursive_calls = local_ctx.number_of_recursive_calls;
			if local_ctx.number_of_recursive_calls > 2048 && local_ctx.are_we_in_true_branch() {
				return Err(miette::Report::new(
					SemanticError::RecursiveModuleInstantiation
						.to_diagnostic_builder()
						.label(
							self.module_name.location,
							format!(
								"Module \"{}\" is instantiated recursively",
								ctx.id_table.get_by_key(&name).unwrap()
							)
							.as_str(),
						)
						.build(),
				));
			}
		}
		debug!("Defining module instance {:?}", module_instance);

		if !local_ctx.scope.is_generic() && scope.is_generic() && local_ctx.are_we_in_true_branch() {
			scope.unmark_as_generic();
			let implementation = ctx.generic_modules.get(&name).unwrap().clone();
			let mut new_local_ctx = LocalAnalyzerContext::new(implementation.id, scope);
			new_local_ctx.number_of_recursive_calls = recursive_calls;
			crate::analyzer::first_pass(ctx, &mut new_local_ctx, &implementation)?;
			new_local_ctx.second_pass(ctx)?;
		}
		let v = Variable::new(
			self.instance_name,
			self.location,
			VariableKind::ModuleInstance(ModuleInstance {
				module_name: self.instance_name,
				location: self.location,
				kind: crate::analyzer::ModuleInstanceKind::Module(module_instance),
			}),
		);
		local_ctx.scope.define_variable(scope_id, v)?;
		return Ok(());
	}

	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContext,
		scope_id: usize,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		use log::*;
		let additional_ctx = AdditionalContext::new(
			local_ctx.nc_widths.clone(),
			local_ctx.array_or_bus.clone(),
			local_ctx.casts.clone(),
		);

		if ctx.id_table.get_value(&self.module_name.get_last_module()).as_str() == "reg" {
			let r = local_ctx.scope.get_variable(scope_id, &self.instance_name).unwrap();
			if let VariableKind::ModuleInstance(m) = &r.var.kind {
				if let ModuleInstanceKind::Register(reg) = &m.kind {
					let builder = hirn::design::RegisterBuilder::new(
						api_scope.clone(),
						&ctx.id_table.get_value(&self.instance_name),
					);
					let clk_var = local_ctx.scope.get_intermidiate_signal(reg.clk).clone();
					let data_var = local_ctx.scope.get_intermidiate_signal(reg.data).clone();
					let next_var = local_ctx.scope.get_intermidiate_signal(reg.next).clone();
					let en_var = local_ctx.scope.get_intermidiate_signal(reg.enable).clone();
					let nreset_var = local_ctx.scope.get_intermidiate_signal(reg.nreset).clone();
					let clk_id = clk_var.var.register(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
						api_scope
							.new_signal(ctx.id_table.get_by_key(&clk_var.var.name).unwrap().as_str())
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(clk_var.var.location, "Error occured here")
									.build()
							})?
							.generated(),
					)?;
					local_ctx.scope.insert_api_id(clk_var.id, clk_id);
					let next_id = next_var.var.register(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
						api_scope
							.new_signal(ctx.id_table.get_by_key(&next_var.var.name).unwrap().as_str())
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(next_var.var.location, "Error occured here")
									.build()
							})?
							.generated(),
					)?;
					local_ctx.scope.insert_api_id(next_var.id, next_id);
					let enable_id = en_var.var.register(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
						api_scope
							.new_signal(ctx.id_table.get_by_key(&en_var.var.name).unwrap().as_str())
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(en_var.var.location, "Error occured here")
									.build()
							})?
							.generated(),
					)?;
					local_ctx.scope.insert_api_id(en_var.id, enable_id);
					let reset_id = nreset_var.var.register(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
						api_scope
							.new_signal(ctx.id_table.get_by_key(&nreset_var.var.name).unwrap().as_str())
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(nreset_var.var.location, "Error occured here")
									.build()
							})?
							.generated(),
					)?;
					local_ctx.scope.insert_api_id(nreset_var.id, reset_id);
					let data_id = data_var.var.register(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
						api_scope
							.new_signal(ctx.id_table.get_by_key(&data_var.var.name).unwrap().as_str())
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(data_var.var.location, "Error occured here")
									.build()
							})?
							.generated(),
					)?;
					local_ctx.scope.insert_api_id(data_var.id, data_id);
					for stmt in &self.port_bind {
						let rhs = stmt.codegen_pass(ctx, local_ctx, api_scope, scope_id)?;
						debug!("Codegen pass for port bind {:?}", stmt);
						match ctx.id_table.get_value(&stmt.get_id()).as_str() {
							"clk" => {
								api_scope
									.assign(hirn::design::Expression::Signal(clk_id.into()), rhs)
									.map_err(|err| {
										CompilerError::HirnApiError(err)
											.to_diagnostic_builder()
											.label(clk_var.var.location, "Error occured here")
											.build()
									})?;
							},
							"next" => {
								api_scope
									.assign(hirn::design::Expression::Signal(next_id.into()), rhs)
									.map_err(|err| {
										CompilerError::HirnApiError(err)
											.to_diagnostic_builder()
											.label(next_var.var.location, "Error occured here")
											.build()
									})?;
							},
							"en" => {
								api_scope
									.assign(hirn::design::Expression::Signal(enable_id.into()), rhs)
									.map_err(|err| {
										CompilerError::HirnApiError(err)
											.to_diagnostic_builder()
											.label(en_var.var.location, "Error occured here")
											.build()
									})?;
							},
							"nreset" => {
								api_scope
									.assign(hirn::design::Expression::Signal(reset_id.into()), rhs)
									.map_err(|err| {
										CompilerError::HirnApiError(err)
											.to_diagnostic_builder()
											.label(nreset_var.var.location, "Error occured here")
											.build()
									})?;
							},
							"data" => {
								api_scope
									.assign(rhs, hirn::design::Expression::Signal(data_id.into()))
									.map_err(|err| {
										CompilerError::HirnApiError(err)
											.to_diagnostic_builder()
											.label(data_var.var.location, "Error occured here")
											.build()
									})?;
							},
							_ => unreachable!(),
						}
					}
					builder
						.next(next_id)
						.en(enable_id)
						.nreset(reset_id)
						.clk(clk_id)
						.output(data_id)
						.build()
						.map_err(|err| {
							CompilerError::HirnApiError(err)
								.to_diagnostic_builder()
								.label(self.location, "Error occured here")
								.build()
						})?;
					return Ok(());
				}
			}
			unreachable!();
		}
		let name = self.module_name.get_last_module();
		let module = ctx.modules_declared.get(&name).unwrap();
		let scope = &module.scope;
		let m_handle = module.handle.clone();
		let module_instance = &local_ctx
			.scope
			.get_variable(scope_id, &self.instance_name)
			.unwrap()
			.var
			.kind
			.clone();
		let m_inst = match module_instance {
			VariableKind::ModuleInstance(m) => &m.kind,
			_ => unreachable!(),
		};
		let m_inst = match &m_inst {
			ModuleInstanceKind::Module(m) => m,
			_ => unreachable!(),
		};
		let mut builder = api_scope
			.new_module(m_handle, &ctx.id_table.get_value(&self.instance_name).as_str())
			.map_err(|err| {
				CompilerError::HirnApiError(err)
					.to_diagnostic_builder()
					.label(self.location, "Error occured here")
					.build()
			})?;
		debug!("Codegen for generic variables");
		for stmt in &self.port_bind {
			let var = scope.get_variable(0, &stmt.get_id()).expect("This was checked");
			if !var.var.kind.is_generic() {
				continue;
			}
			let rhs = stmt.codegen_pass(ctx, local_ctx, api_scope, scope_id)?;
			debug!(
				"Codegen pass for port bind {:?}",
				ctx.id_table.get_value(&stmt.get_id())
			);
			let var = local_ctx
				.scope
				.get_intermidiate_signal(*m_inst.interface.get(&stmt.get_id()).unwrap())
				.clone();
			let var_id: hirn::design::SignalId = var.var.register(
				ctx.nc_table,
				ctx.id_table,
				scope_id,
				&local_ctx.scope,
				Some(&additional_ctx),
				api_scope
					.new_signal(ctx.id_table.get_by_key(&var.var.name).unwrap().as_str())
					.map_err(|err| {
						CompilerError::HirnApiError(err)
							.to_diagnostic_builder()
							.label(var.var.location, "Error occured here")
							.build()
					})?,
			)?;
			local_ctx.scope.insert_api_id(var.id, var_id);
			builder = builder.bind(&ctx.id_table.get_value(&stmt.get_id()).as_str(), var_id);
			api_scope.assign(var_id.into(), rhs).map_err(|err| {
				CompilerError::HirnApiError(err)
					.to_diagnostic_builder()
					.label(var.var.location, "Error occured here")
					.build()
			})?;
			debug!("Assigned succesfuly");
		}
		debug!("Codegen for clocks");
		for stmt in &self.port_bind {
			let var = scope.get_variable(0, &stmt.get_id()).expect("This was checked");
			if !var.var.is_clock() {
				continue;
			}
			let rhs = stmt.codegen_pass(ctx, local_ctx, api_scope, scope_id)?;
			debug!(
				"Codegen pass for port bind {:?}",
				ctx.id_table.get_value(&stmt.get_id())
			);
			let var = &local_ctx
				.scope
				.get_intermidiate_signal(*m_inst.interface.get(&stmt.get_id()).unwrap())
				.clone();
			let var_id: hirn::design::SignalId = var.var.register(
				ctx.nc_table,
				ctx.id_table,
				scope_id,
				&local_ctx.scope,
				Some(&additional_ctx),
				api_scope
					.new_signal(ctx.id_table.get_by_key(&var.var.name).unwrap().as_str())
					.map_err(|err| {
						CompilerError::HirnApiError(err)
							.to_diagnostic_builder()
							.label(var.var.location, "Error occured here")
							.build()
					})?,
			)?;
			local_ctx.scope.insert_api_id(var.id, var_id);
			builder = builder.bind(&ctx.id_table.get_value(&stmt.get_id()).as_str(), var_id);
			api_scope.assign(var_id.into(), rhs).map_err(|err| {
				CompilerError::HirnApiError(err)
					.to_diagnostic_builder()
					.label(var.var.location, "Error occured here")
					.build()
			})?;
			debug!("Assigned succesfuly");
		}
		debug!("Codegen for other variables");
		for stmt in &self.port_bind {
			let interface_variable = scope.get_variable(0, &stmt.get_id()).expect("This was checked");
			let var = scope.get_variable(0, &stmt.get_id()).expect("This was checked");
			if var.var.is_clock() || var.var.kind.is_generic() {
				continue;
			}
			let rhs = stmt.codegen_pass(ctx, local_ctx, api_scope, scope_id)?;
			debug!(
				"Codegen pass for port bind {:?}",
				ctx.id_table.get_value(&stmt.get_id())
			);
			let var = &local_ctx
				.scope
				.get_intermidiate_signal(*m_inst.interface.get(&stmt.get_id()).unwrap())
				.clone();
			let var_id: hirn::design::SignalId = var.var.register(
				ctx.nc_table,
				ctx.id_table,
				scope_id,
				&local_ctx.scope,
				Some(&additional_ctx),
				api_scope
					.new_signal(ctx.id_table.get_by_key(&var.var.name).unwrap().as_str())
					.map_err(|err| {
						CompilerError::HirnApiError(err)
							.to_diagnostic_builder()
							.label(var.var.location, "Error occured here")
							.build()
					})?
					.generated(),
			)?;
			local_ctx.scope.insert_api_id(var.id, var_id);
			builder = builder.bind(&ctx.id_table.get_value(&stmt.get_id()).as_str(), var_id);
			match interface_variable
				.var
				.kind
				.to_signal()
				.expect("This was checked during analysis of a module")
				.direction
			{
				crate::analyzer::Direction::Input(_) => api_scope.assign(var_id.into(), rhs).map_err(|err| {
					CompilerError::HirnApiError(err)
						.to_diagnostic_builder()
						.label(var.var.location, "Error occured here")
						.build()
				})?,
				crate::analyzer::Direction::Output(_) => api_scope.assign(rhs, var_id.into()).map_err(|err| {
					CompilerError::HirnApiError(err)
						.to_diagnostic_builder()
						.label(var.var.location, "Error occured here")
						.build()
				})?,
				_ => unreachable!(),
			}
			debug!("Assigned succesfuly");
		}
		builder.build().map_err(|err| {
			CompilerError::HirnApiError(err)
				.to_diagnostic_builder()
				.label(self.location, "Error occured here")
				.build()
		})?;
		Ok(())
	}
}
fn create_register(
	inst_stmt: &InstantiationStatement,
	scope_id: usize,
	ctx: &mut GlobalAnalyzerContext,
	local_ctx: &mut LocalAnalyzerContext,
) -> miette::Result<RegisterInstance> {
	use log::*;
	if inst_stmt.port_bind.len() != 5 {
		return Err(miette::Report::new(
			InstanceError::ArgumentsMismatch
				.to_diagnostic_builder()
				.label(inst_stmt.location, "Register must have 5 arguments")
				.build(),
		));
	}
	let mut en_stmt: Option<&PortBindStatement> = None;
	let mut next_stmt: Option<&PortBindStatement> = None;
	let mut clk_stmt: Option<&PortBindStatement> = None;
	let mut nreset_stmt: Option<&PortBindStatement> = None;
	let mut data_stmt: Option<&PortBindStatement> = None;
	for stmt in &inst_stmt.port_bind {
		match ctx.id_table.get_value(&stmt.get_id()).as_str() {
			"en" => match en_stmt {
				Some(stmt1) => {
					return Err(miette::Report::new(
						InstanceError::ArgumentsMismatch
							.to_diagnostic_builder()
							.label(stmt.location(), "This argument is already defined")
							.label(stmt1.location(), "En signal is already defined here")
							.build(),
					))
				},
				None => en_stmt = Some(stmt),
			},
			"next" => match next_stmt {
				Some(stmt1) => {
					return Err(miette::Report::new(
						InstanceError::ArgumentsMismatch
							.to_diagnostic_builder()
							.label(stmt.location(), "This argument is already defined")
							.label(stmt1.location(), "Next signal is already defined here")
							.build(),
					))
				},
				None => next_stmt = Some(stmt),
			},
			"clk" => match clk_stmt {
				Some(stmt1) => {
					return Err(miette::Report::new(
						InstanceError::ArgumentsMismatch
							.to_diagnostic_builder()
							.label(stmt.location(), "This argument is already defined")
							.label(stmt1.location(), "Clk signal is already defined here")
							.build(),
					))
				},
				None => clk_stmt = Some(stmt),
			},
			"nreset" => match nreset_stmt {
				Some(stmt1) => {
					return Err(miette::Report::new(
						InstanceError::ArgumentsMismatch
							.to_diagnostic_builder()
							.label(stmt.location(), "This argument is already defined")
							.label(stmt1.location(), "NReset signal is already defined here")
							.build(),
					))
				},
				None => nreset_stmt = Some(stmt),
			},
			"data" => match data_stmt {
				Some(stmt1) => {
					return Err(miette::Report::new(
						InstanceError::ArgumentsMismatch
							.to_diagnostic_builder()
							.label(stmt.location(), "This argument is already defined")
							.label(stmt1.location(), "Data signal is already defined here")
							.build(),
					))
				},
				None => data_stmt = Some(stmt),
			},
			_ => {
				return Err(miette::Report::new(
					InstanceError::ArgumentsMismatch
						.to_diagnostic_builder()
						.label(stmt.location(), "This is not a valid argument for register")
						.build(),
				))
			},
		};
	}
	use crate::parser::ast::PortBindStatement::*;
	let mut clk_type = clk_stmt
		.unwrap()
		.get_type(ctx, local_ctx, scope_id, Signal::new_empty(), false)?;
	clk_type.sensitivity = SignalSensitivity::Clock(clk_stmt.unwrap().location(), None);
	debug!("Clk type is {:?}", clk_type);
	if clk_type.is_array() {
		return Err(miette::Report::new(
			InstanceError::ArgumentsMismatch
				.to_diagnostic_builder()
				.label(clk_stmt.unwrap().location(), "Clock cannot be array")
				.build(),
		));
	}

	let inst_name = ctx.id_table.get_value(&inst_stmt.instance_name).clone();
	let clk_name = ctx.id_table.insert_or_get(format!("{}_clk", inst_name).as_str());
	let next_name = ctx.id_table.insert_or_get(format!("{}_nxt_c", inst_name).as_str());
	let en_name = ctx.id_table.insert_or_get(format!("{}_en_c", inst_name).as_str());
	let nreset_name = ctx.id_table.insert_or_get(format!("{}_nreset", inst_name).as_str());
	let data_name = ctx.id_table.insert_or_get(format!("{}_out_r", inst_name).as_str());

	debug!("Clk type is {:?}", clk_type);
	let clk_var_id = local_ctx.scope.define_intermidiate_signal(Variable::new(
		clk_name,
		next_stmt.unwrap().location(),
		VariableKind::Signal(clk_type),
	))?;
	let mut data_type = data_stmt
		.unwrap()
		.get_type(ctx, local_ctx, scope_id, Signal::new_empty(), true)?;
	data_type.sensitivity = SignalSensitivity::Sync(
		ClockSensitivityList::new().with_clock(clk_var_id, true, data_stmt.unwrap().location()),
		data_stmt.unwrap().location(),
	);
	debug!("Data type is {:?}", data_type);
	if data_type.is_array() {
		return Err(miette::Report::new(
			InstanceError::ArgumentsMismatch
				.to_diagnostic_builder()
				.label(data_stmt.unwrap().location(), "Data signal cannot be array")
				.build(),
		));
	}
	debug!("Data type is {:?}", data_type);
	let mut next_type = next_stmt
		.unwrap()
		.get_type(ctx, local_ctx, scope_id, data_type.clone(), false)?;
	next_type.sensitivity = SignalSensitivity::Comb(
		ClockSensitivityList::new().with_clock(clk_var_id, true, next_stmt.unwrap().location()),
		next_stmt.unwrap().location(),
	);
	debug!("Next type is {:?}", next_type);
	if next_type.is_array() {
		return Err(miette::Report::new(
			InstanceError::ArgumentsMismatch
				.to_diagnostic_builder()
				.label(next_stmt.unwrap().location(), "Next signal cannot cannot be array")
				.build(),
		));
	}
	match (data_type.is_width_specified(), next_type.is_width_specified()) {
		(true, true) => {
			data_type.evaluate_as_lhs(false, &ctx, next_type.clone(), data_stmt.unwrap().location())?;
		},
		(true, false) => {
			next_type.evaluate_as_lhs(false, &ctx, data_type.clone(), next_stmt.unwrap().location())?;
			match next_stmt.unwrap() {
				OnlyId(id) => {
					let mut sig = local_ctx.scope.get_variable(scope_id, &id.id).unwrap().clone();
					sig.var.kind = VariableKind::Signal(next_type.clone());
					local_ctx.scope.redeclare_variable(sig);
				},
				IdWithExpression(expr) => {
					expr.expression.evaluate_type(
						ctx,
						scope_id,
						local_ctx,
						data_type.clone(),
						false,
						next_stmt.unwrap().location(),
					)?;
				},
				IdWithDeclaration(_) => todo!(),
			};
		},
		(false, true) => {
			data_type.evaluate_as_lhs(false, &ctx, next_type.clone(), data_stmt.unwrap().location())?;
			match data_stmt.unwrap() {
				OnlyId(id) => {
					let mut sig = local_ctx.scope.get_variable(scope_id, &id.id).unwrap().clone();
					sig.var.kind = VariableKind::Signal(data_type.clone());
					local_ctx.scope.redeclare_variable(sig);
				},
				IdWithExpression(expr) => {
					expr.expression.evaluate_type(
						ctx,
						scope_id,
						local_ctx,
						next_type.clone(),
						false,
						data_stmt.unwrap().location(),
					)?;
				},
				IdWithDeclaration(_) => todo!(),
			};
		},
		(false, false) => {
			return Err(miette::Report::new(
				InstanceError::ArgumentsMismatch
					.to_diagnostic_builder()
					.label(next_stmt.unwrap().location(), "Next signal must have width specified")
					.build(),
			));
		},
	}
	match (data_type.is_signedness_specified(), next_type.is_signedness_specified()) {
		(true, true) => {
			data_type.evaluate_as_lhs(false, &ctx, next_type.clone(), data_stmt.unwrap().location())?;
		},
		(true, false) => (), //FIXME
		(false, true) => (), //FIXME
		(false, false) => {
			return Err(miette::Report::new(
				InstanceError::ArgumentsMismatch
					.to_diagnostic_builder()
					.label(
						next_stmt.unwrap().location(),
						"Next signal must have signedness specified",
					)
					.build(),
			))
		},
	};
	let mut nreset_type = nreset_stmt
		.unwrap()
		.get_type(ctx, local_ctx, scope_id, Signal::new_empty(), false)?;
	nreset_type.sensitivity = SignalSensitivity::Async(nreset_stmt.unwrap().location());
	let mut en_type = en_stmt
		.unwrap()
		.get_type(ctx, local_ctx, scope_id, Signal::new_empty(), false)?;
	en_type.sensitivity = SignalSensitivity::Comb(
		ClockSensitivityList::new().with_clock(clk_var_id, true, en_stmt.unwrap().location()),
		en_stmt.unwrap().location(),
	);
	debug!("En type is {:?}", en_type);
	let en_var_id = local_ctx.scope.define_intermidiate_signal(Variable::new(
		en_name,
		next_stmt.unwrap().location(),
		VariableKind::Signal(en_type),
	))?;
	debug!("Nreset type is {:?}", nreset_type);
	let nreset_var_id = local_ctx.scope.define_intermidiate_signal(Variable::new(
		nreset_name,
		next_stmt.unwrap().location(),
		VariableKind::Signal(nreset_type),
	))?;
	debug!("Data type is {:?}", data_type);
	let data_var_id = local_ctx.scope.define_intermidiate_signal(Variable::new(
		data_name,
		next_stmt.unwrap().location(),
		VariableKind::Signal(data_type.clone()),
	))?;
	debug!("Next type is {:?}", next_type);
	let next_var_id = local_ctx.scope.define_intermidiate_signal(Variable::new(
		next_name,
		next_stmt.unwrap().location(),
		VariableKind::Signal(next_type),
	))?;
	local_ctx
		.sensitivity_graph
		.add_edges(
			en_stmt.unwrap().get_sensitivity_entry(&ctx, local_ctx, scope_id),
			SensitivityGraphEntry::Signal(en_var_id, en_stmt.unwrap().location()),
			en_stmt.unwrap().location(),
		)
		.map_err(|e| e.build())?;
	local_ctx
		.sensitivity_graph
		.add_edges(
			nreset_stmt.unwrap().get_sensitivity_entry(&ctx, local_ctx, scope_id),
			SensitivityGraphEntry::Signal(nreset_var_id, nreset_stmt.unwrap().location()),
			nreset_stmt.unwrap().location(),
		)
		.map_err(|e| e.build())?;
	local_ctx
		.sensitivity_graph
		.add_edges(
			next_stmt.unwrap().get_sensitivity_entry(&ctx, local_ctx, scope_id),
			SensitivityGraphEntry::Signal(next_var_id, next_stmt.unwrap().location()),
			next_stmt.unwrap().location(),
		)
		.map_err(|e| e.build())?;
	local_ctx
		.sensitivity_graph
		.add_edges(
			clk_stmt.unwrap().get_sensitivity_entry(&ctx, local_ctx, scope_id),
			SensitivityGraphEntry::Signal(clk_var_id, clk_stmt.unwrap().location()),
			clk_stmt.unwrap().location(),
		)
		.map_err(|e| e.build())?;
	let (id, loc) = data_stmt.unwrap().get_internal_id(&local_ctx.scope, scope_id);
	local_ctx
		.sensitivity_graph
		.add_edges(
			vec![SensitivityGraphEntry::Signal(
				data_var_id,
				data_stmt.unwrap().location(),
			)],
			SensitivityGraphEntry::Signal(id, loc),
			data_stmt.unwrap().location(),
		)
		.map_err(|e| e.build())?;
	let r = RegisterInstance {
		name: inst_stmt.instance_name,
		location: inst_stmt.location,
		next: next_var_id,
		clk: clk_var_id,
		nreset: nreset_var_id,
		data: data_var_id,
		enable: en_var_id,
	};
	Ok(r)
}
