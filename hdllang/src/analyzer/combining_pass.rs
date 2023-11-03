use super::{AlreadyCreated, ModuleDeclared, RegisterInstance, SemanticError, Variable, VariableKind};
use hirn::design::{ScopeHandle, UnaryExpression};
use log::{debug, info};
use std::collections::HashMap;
use std::io::Write;

use crate::{
	analyzer::{
		semantic_error::InstanceError, BusWidth, ClockSensitivityList, GenericVariable, ModuleImplementationScope,
		ModuleInstance, ModuleInstanceKind, NonRegister, SensitivityGraphEntry, Signal, SignalSensitivity, module_implementation_scope::InternalVariableId,
	},
	core::*,
	parser::ast::*,
	CompilerError, ProvidesCompilerDiagnostic, SourceSpan,
};

pub fn combine<'a>(
	id_table: &'a mut IdTable,
	nc_table: &'a crate::lexer::NumericConstantTable,
	ast: &'a Root,
	mut path_from_root: String,
	present_files: &mut HashMap<String, String>,
) -> miette::Result<(
	Vec<String>,
	GlobalAnalyzerContext<'a>,
	HashMap<IdTableKey, &'a ModuleImplementation>,
)> {
	info!("Running combining pass");
	info!("Path from root: {}", path_from_root);

	if path_from_root.as_str() == "" {
		path_from_root = ".".to_string();
	}
	let mut design = hirn::design::DesignHandle::new();
	let mut packaged_paths: Vec<String> = Vec::new();
	let mut modules_declared: HashMap<IdTableKey, ModuleDeclared> = HashMap::new();
	let mut modules_implemented: HashMap<IdTableKey, &ModuleImplementation> = HashMap::new();
	let mut generic_modules: HashMap<IdTableKey, &ModuleImplementation> = HashMap::new();

	for def in &ast.definitions {
		use crate::parser::ast::TopDefinition::*;
		match def {
			ModuleDeclaration(declaration) => {
				declaration.analyze(&mut design, id_table, nc_table, &mut modules_declared)?
			},
			ModuleImplementation(implementation) => {
				debug!(
					"Found module impl for {:?}",
					id_table.get_by_key(&implementation.id).unwrap()
				);
				if let Some(prev) = modules_implemented.get(&implementation.id) {
					return Err(miette::Report::new(
						SemanticError::MultipleModuleImplementations
							.to_diagnostic_builder()
							.label(
								implementation.location,
								format!(
									"Module {:?} is implemented multiple times",
									id_table.get_by_key(&implementation.id).unwrap()
								)
								.as_str(),
							)
							.label(
								prev.location,
								format!(
									"Module {:?} is implemented here",
									id_table.get_by_key(&prev.id).unwrap()
								)
								.as_str(),
							)
							.build(),
					));
				}

				modules_implemented.insert(implementation.id, &implementation);
			},
			PackageDeclaration(package) => {
				packaged_paths.push(package.analyze(id_table, &path_from_root, present_files)?)
			},
			UseStatement(_) => (),
		};
	}
	for (name, m) in modules_declared.clone() {
		if m.is_generic {
			match modules_implemented.get(&name) {
				None => {
					return Err(miette::Report::new(
						crate::ProvidesCompilerDiagnostic::to_diagnostic_builder(
							&crate::analyzer::SemanticError::GenericModuleImplementationNotFound,
						)
						.label(
							m.location,
							format!(
								"Module {:?} is not implemented in the same file as it is declared",
								id_table.get_by_key(&name).unwrap()
							)
							.as_str(),
						)
						.build(),
					))
				},
				Some(implementation) => {
					generic_modules.insert(name.clone(), *implementation);
					modules_implemented.remove(&name);
				},
			}
		}
	}
	debug!("Modules: {:?}", modules_declared.len());

	// prepare for next stage of analysis
	let ctx: GlobalAnalyzerContext<'_> = GlobalAnalyzerContext {
		id_table,
		nc_table,
		modules_declared,
		generic_modules,
		design,
	};

	Ok((packaged_paths, ctx, modules_implemented))
}
pub struct SemanticalAnalyzer<'a> {
	ctx: GlobalAnalyzerContext<'a>,
	modules_implemented: &'a HashMap<IdTableKey, &'a ModuleImplementation>,
	passes: Vec<
		for<'b> fn(
			&mut GlobalAnalyzerContext<'a>,
			&mut LocalAnalyzerContex,
			&ModuleImplementation,
		) -> miette::Result<()>,
	>,
}
impl<'a> SemanticalAnalyzer<'a> {
	pub fn new(
		ctx: GlobalAnalyzerContext<'a>,
		modules_implemented: &'a HashMap<IdTableKey, &ModuleImplementation>,
	) -> Self {
		Self {
			ctx,
			modules_implemented,
			passes: Vec::new(),
		}
	}
	pub fn semantical_analysis(&mut self) -> miette::Result<()> {
		self.passes.push(first_pass);
		self.passes.push(second_pass);
		for module in self.modules_implemented.values() {
			let mut local_ctx = LocalAnalyzerContex::new(
				module.id,
				self.ctx.modules_declared.get(&module.id).unwrap().scope.clone(),
			);
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}
		}
		Ok(())
	}
	pub fn compile_and_elaborate(&mut self, output: &mut dyn Write) -> miette::Result<()> {
		self.passes.push(first_pass);
		self.passes.push(second_pass);
		self.passes.push(codegen_pass);
		for module in self.modules_implemented.values() {
			let mut local_ctx = LocalAnalyzerContex::new(
				module.id,
				self.ctx.modules_declared.get(&module.id).unwrap().scope.clone(),
			);
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}
			todo!("Invoke elaboration");
			let mut output_string: String = String::new();
			let mut sv_codegen = hirn::codegen::sv::SVCodegen::new(self.ctx.design, &mut output_string);
			use hirn::codegen::Codegen;
			sv_codegen
				.emit_module(
					self.ctx
						.modules_declared
						.get_mut(&local_ctx.module_id)
						.unwrap()
						.handle
						.id(),
				)
				.unwrap();
			write!(output, "{}", output_string).unwrap();
		}
		Ok(())
	}
	pub fn compile(&mut self, output: &mut dyn Write) -> miette::Result<()> {
		self.passes.push(first_pass);
		self.passes.push(second_pass);
		self.passes.push(codegen_pass);
		for module in self.modules_implemented.values() {
			let mut local_ctx = LocalAnalyzerContex::new(
				module.id,
				self.ctx.modules_declared.get(&module.id).unwrap().scope.clone(),
			);
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}
			let mut output_string = String::new();
			let mut sv_codegen = hirn::codegen::sv::SVCodegen::new(self.ctx.design.clone(), &mut output_string);
			use hirn::codegen::Codegen;
			sv_codegen
				.emit_module(
					self.ctx
						.modules_declared
						.get_mut(&local_ctx.module_id)
						.unwrap()
						.handle
						.id(),
				)
				.unwrap();
			write!(output, "{}", output_string).unwrap();
		}
		Ok(())
	}
}
/// This pass collects all variables
pub fn first_pass(
	ctx: &mut GlobalAnalyzerContext,
	local_ctx: &mut LocalAnalyzerContex,
	module: &ModuleImplementation,
) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running initial pass");
	module.first_pass(ctx, local_ctx)?;
	debug!("Initial pass done");
	Ok(())
}
/// This pass checks if all variables have specified sensitivity and width if needed
pub fn second_pass(
	ctx: &mut GlobalAnalyzerContext,
	local_ctx: &mut LocalAnalyzerContex,
	module: &ModuleImplementation,
) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running second pass");
	module.second_pass(ctx, local_ctx)?;
	debug!("Second pass done");
	Ok(())
}
/// This pass invokes HIRN API and creates proper module
pub fn codegen_pass(
	ctx: &mut GlobalAnalyzerContext,
	local_ctx: &mut LocalAnalyzerContex,
	module: &ModuleImplementation,
) -> miette::Result<()> {
	module.codegen_pass(ctx, local_ctx)?;
	Ok(())
}
use crate::core::NumericConstantTable;
/// Global shared context for semantic analysis
pub struct GlobalAnalyzerContext<'a> {
	pub id_table: &'a mut IdTable,
	pub nc_table: &'a NumericConstantTable,
	/// represents all declared modules
	pub modules_declared: HashMap<IdTableKey, ModuleDeclared>,
	/// represents all implemented generic modules
	pub generic_modules: HashMap<IdTableKey, &'a ModuleImplementation>,
	pub design: hirn::design::DesignHandle,
}
pub struct AdditionalContext {
	pub nc_widths: HashMap<SourceSpan, NumericConstant>,
	pub array_or_bus: HashMap<SourceSpan, bool>, // to distinguish between array and bus in index expr
}
impl AdditionalContext {
	pub fn new(nc_widths: HashMap<SourceSpan, NumericConstant>, array_or_bus: HashMap<SourceSpan, bool>) -> Self {
		AdditionalContext {
			nc_widths,
			array_or_bus,
		}
	}
}
/// Per module context for semantic analysis
pub struct LocalAnalyzerContex {
	pub are_we_in_conditional: usize,
	pub scope: ModuleImplementationScope,
	pub nc_widths: HashMap<SourceSpan, NumericConstant>,
	pub array_or_bus: HashMap<SourceSpan, bool>, // to distinguish between array and bus in index expr
	pub casts: HashMap<SourceSpan, Signal>,
	pub widths_map: HashMap<SourceSpan, BusWidth>,
	pub scope_map: HashMap<SourceSpan, usize>,
	pub module_id: IdTableKey,
	pub sensitivity_graph: super::SensitivityGraph,
	pub are_we_in_true_branch: Vec<bool>,
	pub number_of_recursive_calls: usize,
}
impl LocalAnalyzerContex {
	pub fn new(module_id: IdTableKey, scope: ModuleImplementationScope) -> Self {
		LocalAnalyzerContex {
			are_we_in_conditional: 0,
			scope,
			scope_map: HashMap::new(),
			module_id,
			nc_widths: HashMap::new(),
			widths_map: HashMap::new(),
			sensitivity_graph: super::SensitivityGraph::new(),
			casts: HashMap::new(),
			are_we_in_true_branch: vec![true], // initial value is true :)
			number_of_recursive_calls: 0,
			array_or_bus: HashMap::new(),
		}
	}
	pub fn second_pass(&mut self, ctx: &GlobalAnalyzerContext) -> miette::Result<()> {
		debug!("Second pass");
		self.sensitivity_graph.verify(&mut self.scope, ctx)?;
		self.scope.second_pass(ctx)?;
		Ok(())
	}
}
impl ModuleImplementation {
	// Performs first pass on module implementation
	pub fn first_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
	) -> miette::Result<()> {
		debug!(
			"Analyzing module implementation {}",
			ctx.id_table.get_by_key(&self.id).unwrap()
		);
		use crate::parser::ast::ModuleImplementationStatement::*;
		match ctx.modules_declared.get(&self.id) {
			Some(_) => (),
			None => {
				return Err(miette::Report::new(
					SemanticError::ModuleNotDeclared
						.to_diagnostic_builder()
						.label(
							self.location,
							format!(
								"Declaration of {:?} module cannot be found",
								ctx.id_table.get_by_key(&self.id).unwrap()
							)
							.as_str(),
						)
						.build(),
				))
			},
		}

		// This has to be done this way to avoid creating a inner scope with the first block
		let id = 0;
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
		local_ctx: &mut LocalAnalyzerContex,
	) -> miette::Result<()> {
		local_ctx.second_pass(ctx)?;
		Ok(())
	}

	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
	) -> miette::Result<()> {
		debug!(
			"Codegen pass for module implementation {}",
			ctx.id_table.get_by_key(&self.id).unwrap()
		);
		//let module  = ctx.modules_declared.get(&self.id).unwrap();
		let mut api_scope = ctx
			.modules_declared
			.get_mut(&local_ctx.module_id)
			.unwrap()
			.handle
			.scope();

		//for var in module.scope.signals.values() {
		//	// create variable with API
		//	var.register(ctx, local_ctx, &mut api_scope)?;
		//}

		use crate::parser::ast::ModuleImplementationStatement::*;
		match &self.statement {
			ModuleImplementationBlockStatement(block) => {
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
impl ModuleImplementationStatement {
	pub fn first_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		scope_id: usize,
	) -> miette::Result<()> {
		local_ctx.scope_map.insert(self.get_location(), scope_id);
		info!("Inserting scope id {} for {:?}", scope_id, self.get_location(),);
		use ModuleImplementationStatement::*;
		match self {
			VariableBlock(block) => block.analyze(ctx, local_ctx, AlreadyCreated::new(), scope_id)?,
			VariableDefinition(definition) => definition.analyze(AlreadyCreated::new(), ctx, local_ctx, scope_id)?,
			AssignmentStatement(assignment) => {
				debug!("Assignment takes place in {:?} scope", scope_id);
				if !assignment.lhs.is_lvalue() {
					report_not_allowed_lhs(assignment.lhs.get_location())?;
				}
				if assignment.lhs.is_generic(ctx, scope_id, local_ctx)? {
					if local_ctx.are_we_in_conditional > 0 {
						return Err(miette::Report::new(
							SemanticError::GenericInConditional
								.to_diagnostic_builder()
								.label(
									assignment.location,
									"Generic variable cannot be assigned in conditional statement",
								)
								.build(),
						));
					}
					debug!("Lhs is generic");
					match assignment.rhs.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)? {
						Some(val) => assignment.lhs.assign(
							BusWidth::EvaluatedLocated(val, assignment.rhs.get_location()),
							local_ctx,
							scope_id,
						),
						None => assignment.lhs.assign(
							BusWidth::Evaluable(assignment.rhs.get_location()),
							local_ctx,
							scope_id,
						),
					}
					.map_err(|e| e.label(self.get_location(), "This assignment is invalid").build())?;
					return Ok(());
				}
				let lhs_type = assignment.lhs.evaluate_type(
					ctx,
					scope_id,
					local_ctx,
					Signal::new_empty(),
					true,
					assignment.location,
				)?;
				info!("Lhs type at the beginning: {:?}", lhs_type);
				if lhs_type.is_array() {
					return Err(miette::Report::new(
						SemanticError::ArrayInExpression
							.to_diagnostic_builder()
							.label(assignment.location, "Array assignment is not supported yet")
							.build(),
					));
				}
				let rhs_type =
					assignment
						.rhs
						.evaluate_type(ctx, scope_id, local_ctx, lhs_type, false, assignment.location)?;
				if rhs_type.is_array() {
					return Err(miette::Report::new(
						SemanticError::ArrayInExpression
							.to_diagnostic_builder()
							.label(assignment.location, "Array assignment is not supported yet")
							.label(assignment.rhs.get_location(), "This expression is an array")
							.build(),
					));
				}
				info!("Rhs type at the end: {:?}", rhs_type);
				let new_lhs =
					assignment
						.lhs
						.evaluate_type(ctx, scope_id, local_ctx, rhs_type, true, assignment.location)?;
				let (left_id, loc) = assignment.lhs.get_internal_id(&local_ctx.scope, scope_id);
				let entries = assignment.rhs.get_sensitivity_entry(ctx, &local_ctx.scope, scope_id);
				debug!("Adding edges {:?} to {:?}", entries, left_id);
				local_ctx
					.sensitivity_graph
					.add_edges(
						entries,
						crate::analyzer::SensitivityGraphEntry::Signal(left_id, loc),
						self.get_location(),
					)
					.map_err(|e| e.build())?;
				info!("Lhs type at the and: {:?}", new_lhs);
			},
			IfElseStatement(conditional) => {
				let condition_type = conditional
					.condition
					.evaluate(ctx.nc_table, scope_id, &mut local_ctx.scope)?;
				let if_scope = local_ctx.scope.new_scope(Some(scope_id));
				local_ctx.are_we_in_conditional += 1;
				debug!("Condition is {:?}", condition_type);
				let cond = condition_type.unwrap().value != num_bigint::BigInt::from(0);
				local_ctx.are_we_in_true_branch.push(cond);
				conditional.if_statement.first_pass(ctx, local_ctx, if_scope)?;
				local_ctx.are_we_in_true_branch.pop();
				match &conditional.else_statement {
					Some(stmt) => {
						let else_scope = local_ctx.scope.new_scope(Some(scope_id));
						local_ctx.are_we_in_true_branch.push(!cond);
						stmt.first_pass(ctx, local_ctx, else_scope)?;
						local_ctx.are_we_in_true_branch.pop();
					},
					None => (),
				}
				local_ctx.are_we_in_conditional -= 1;
			},
			IterationStatement(iteration) => {
				local_ctx.are_we_in_conditional += 1;
				let id = local_ctx.scope.new_scope(Some(scope_id));
				let mut initial_val = iteration
					.range
					.lhs
					.evaluate(ctx.nc_table, scope_id, &mut local_ctx.scope)?
					.unwrap()
					.value;
				let mut end_val = iteration
					.range
					.rhs
					.evaluate(ctx.nc_table, scope_id, &mut local_ctx.scope)?
					.unwrap()
					.value;
				use crate::parser::ast::RangeOpcode::*;
				match &iteration.range.code {
					Colon => (),
					PlusColon => end_val += initial_val.clone(),
					ColonLessThan => end_val -= 1,
				}
				local_ctx.scope_map.insert(iteration.statement.get_location(), id);
				while initial_val <= end_val {
					local_ctx.scope.define_variable(
						id,
						Variable::new(
							iteration.id,
							iteration.location,
							VariableKind::Generic(GenericVariable {
								value: Some(BusWidth::Evaluated(NumericConstant::new_from_value(
									initial_val.clone(),
								))),
								direction: crate::analyzer::Direction::None,
								dimensions: Vec::new(),
								kind: crate::analyzer::GenericVariableKind::Int(
									crate::analyzer::SignalSignedness::NoSignedness,
									iteration.location,
								),
							}),
						),
					)?;
					match iteration.statement.as_ref() {
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

				local_ctx.are_we_in_conditional -= 1;
			},
			InstantiationStatement(inst) => {
				let name = inst.module_name.get_last_module();
				match local_ctx.scope.is_declared(scope_id, &inst.instance_name) {
					Some(location) => {
						return Err(miette::Report::new(
							SemanticError::DuplicateVariableDeclaration
								.to_diagnostic_builder()
								.label(
									inst.location,
									format!(
										"Variable \"{}\" is already declared in this scope",
										ctx.id_table.get_by_key(&inst.instance_name).unwrap()
									)
									.as_str(),
								)
								.label(location, "Previous declaration")
								.build(),
						))
					},
					None => (),
				}
				if ctx.id_table.get_value(&name).as_str() == "register" {
					let v = Variable::new(
						inst.instance_name,
						inst.location,
						VariableKind::ModuleInstance(ModuleInstance {
							module_name: inst.instance_name,
							location: inst.location,
							kind: crate::analyzer::ModuleInstanceKind::Register(create_register(
								inst, scope_id, ctx, local_ctx,
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
								inst.module_name.location,
								format!("Module \"{}\" is not declared", ctx.id_table.get_by_key(&name).unwrap())
									.as_str(),
							)
							.build(),
					));
				}
				let instance_str = ctx.id_table.get_by_key(&inst.instance_name).unwrap().clone();
				let module = ctx.modules_declared.get(&name).unwrap();
				let mut scope = module.scope.clone();
				let mut module_instance = NonRegister::new();
				let mut clock_mapping: HashMap<InternalVariableId, InternalVariableId> = HashMap::new();
				if scope.get_interface_len() != inst.port_bind.len() {
					return Err(miette::Report::new(
						InstanceError::ArgumentsMismatch
							.to_diagnostic_builder()
							.label(
								inst.location,
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
				debug!("Binding generic variables!");
				for stmt in &inst.port_bind {
					let mut interface_variable = scope
						.get_var(0, &stmt.get_id())
						.map_err(|mut err| {
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
									if gen1.value.is_none() {
										return Err(miette::Report::new(InstanceError::GenericArgumentWithoutValue.to_diagnostic_builder()
										.label(local_sig.var.location, "This variable does not have a value")
										.label(stmt.location(), "Here there was an attempt to bind generic variable without a value")
										.build()));
									}
									gen2.value = gen1.value.clone();
									let new_var =
										Variable::new(new_name, stmt.location(), interface_variable.var.kind.clone());
									scope.redeclare_variable(interface_variable);
									let id = local_ctx.scope.define_intermidiate_signal(new_var)?;
									module_instance.add_variable(stmt.get_id(), id).map_err(|mut err| {
										err.label(stmt.location(), "Variable declared here").build()
									})?;
								},
								(..) => unreachable!(),
							}
						},
						IdWithExpression(id_expr) => {
							debug!("Id with expression");

							let new_sig = id_expr.expression.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)?;
							if new_sig.is_none() {
								todo!() //FIXME
							}
							use VariableKind::*;
							match &mut interface_variable.var.kind {
								Generic(gen) => {
									gen.value = Some(BusWidth::Evaluated(new_sig.unwrap()));
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
				debug!("Binding clocks!");
				for stmt in &inst.port_bind {
					let sig_name = ctx.id_table.get_value(&stmt.get_id()).clone();
					let new_name_str = format!("{instance_str}_{sig_name}_clk");
					let new_name = ctx.id_table.insert_or_get(new_name_str.as_str());
					let mut interface_variable =
						scope.get_variable(0, &stmt.get_id()).expect("This was checked").clone();
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
					if let SignalSensitivity::Clock(_, Some(name)) = coming.sensitivity {
						clock_mapping.insert(clk_type.get_clock_name(), name);
						coming.evaluate_as_lhs(false, ctx, clk_type, stmt.location())?;
						debug!("Adding variable {:?}", new_name_str);
						let new_id = local_ctx.scope.define_intermidiate_signal(Variable::new(
							new_name,
							stmt.location(),
							VariableKind::Signal(coming),
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
							let entries = stmt.get_sensitivity_entry(ctx, &local_ctx.scope, scope_id);
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
				for stmt in &inst.port_bind {
					let sig_name = ctx.id_table.get_value(&stmt.get_id()).clone();
					let new_name_str = format!("{instance_str}_{sig_name}");
					let new_name = ctx.id_table.insert_or_get(new_name_str.as_str());
					let mut interface_variable =
						scope.get_variable(0, &stmt.get_id()).expect("This was checked").clone();
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
					debug!("Interface variable is {:?}", interface_variable.var.kind);
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
						let entries = stmt.get_sensitivity_entry(ctx, &local_ctx.scope, scope_id);
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
					local_ctx.number_of_recursive_calls += 1;
					recursive_calls = local_ctx.number_of_recursive_calls;
					if local_ctx.number_of_recursive_calls > 2048
						&& local_ctx.are_we_in_true_branch.last().unwrap().clone()
					{
						return Err(miette::Report::new(
							SemanticError::RecursiveModuleInstantiation
								.to_diagnostic_builder()
								.label(
									inst.module_name.location,
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

				if scope.is_generic() && local_ctx.are_we_in_true_branch.last().unwrap().clone() {
					scope.unmark_as_generic();
					let implementation = ctx.generic_modules.get(&name).unwrap().clone();
					let mut new_local_ctx = LocalAnalyzerContex::new(implementation.id, scope);
					new_local_ctx.number_of_recursive_calls = recursive_calls;
					first_pass(ctx, &mut new_local_ctx, &implementation)?;
					new_local_ctx.second_pass(ctx)?;
				}
				let v = Variable::new(
					inst.instance_name,
					inst.location,
					VariableKind::ModuleInstance(ModuleInstance {
						module_name: inst.instance_name,
						location: inst.location,
						kind: crate::analyzer::ModuleInstanceKind::Module(module_instance),
					}),
				);
				local_ctx.scope.define_variable(scope_id, v)?;
				return Ok(());
			},
			ModuleImplementationBlockStatement(block) => {
				//let id = local_ctx.scope.new_scope(Some(scope_id));
				block.analyze(ctx, local_ctx, scope_id)?;
			},
		}
		Ok(())
	}
	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		info!("Reading scope id for {:?}", self.get_location());
		let scope_id = local_ctx.scope_map.get(&self.get_location()).unwrap().to_owned();
		let additional_ctx = AdditionalContext::new(local_ctx.nc_widths.clone(), local_ctx.array_or_bus.clone());
		use ModuleImplementationStatement::*;
		match self {
			VariableBlock(block) => block.codegen_pass(ctx, local_ctx, api_scope)?,
			VariableDefinition(definition) => definition.codegen_pass(ctx, local_ctx, api_scope)?,
			AssignmentStatement(assignment) => {
				let lhs = assignment.lhs.codegen(
					ctx.nc_table,
					ctx.id_table,
					scope_id,
					&local_ctx.scope,
					Some(&additional_ctx),
				)?;
				let rhs = assignment.rhs.codegen(
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
						.label(self.get_location(), "Error occured here")
						.build()
				})?;
				debug!("Assignment done");
			},
			IfElseStatement(conditional) => {
				let condition_expr = conditional.condition.codegen(
					ctx.nc_table,
					ctx.id_table,
					scope_id,
					&local_ctx.scope,
					Some(&additional_ctx),
				)?;
				debug!("{:?}", condition_expr);
				match conditional.else_statement {
					Some(ref else_stmt) => {
						let (mut if_scope, mut else_scope) =
							api_scope.if_else_scope(condition_expr).map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(self.get_location(), "Error occured here")
									.build()
							})?;
						conditional.if_statement.codegen_pass(ctx, local_ctx, &mut if_scope)?;
						else_stmt.codegen_pass(ctx, local_ctx, &mut else_scope)?;
					},
					None => {
						let mut if_scope = api_scope.if_scope(condition_expr).map_err(|err| {
							CompilerError::HirnApiError(err)
								.to_diagnostic_builder()
								.label(self.get_location(), "Error occured here")
								.build()
						})?;
						conditional.if_statement.codegen_pass(ctx, local_ctx, &mut if_scope)?;
					},
				}
				let mut inner_scope = api_scope
					.if_scope(conditional.condition.codegen(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
					)?)
					.map_err(|err| {
						CompilerError::HirnApiError(err)
							.to_diagnostic_builder()
							.label(self.get_location(), "Error occured here")
							.build()
					})?;
				conditional
					.if_statement
					.codegen_pass(ctx, local_ctx, &mut inner_scope)?;
				match conditional.else_statement {
					Some(ref else_statement) => {
						let expr = conditional.condition.codegen(
							ctx.nc_table,
							ctx.id_table,
							scope_id,
							&local_ctx.scope,
							Some(&additional_ctx),
						)?;
						let mut else_scope = api_scope
							.if_scope(hirn::design::Expression::Unary(UnaryExpression {
								op: hirn::design::UnaryOp::LogicalNot,
								operand: Box::new(expr),
							}))
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(self.get_location(), "Error occured here")
									.build()
							})?;
						else_statement.codegen_pass(ctx, local_ctx, &mut else_scope)?
					},
					None => (),
				}
			},
			IterationStatement(for_stmt) => {
				let lhs = for_stmt.range.lhs.codegen(
					ctx.nc_table,
					ctx.id_table,
					scope_id,
					&local_ctx.scope,
					Some(&additional_ctx),
				)?;
				let mut rhs = for_stmt.range.rhs.codegen(
					ctx.nc_table,
					ctx.id_table,
					scope_id,
					&local_ctx.scope,
					Some(&additional_ctx),
				)?;
				use RangeOpcode::*;
				match &for_stmt.range.code {
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
					.loop_scope(&ctx.id_table.get_value(&for_stmt.id), lhs, rhs)
					.map_err(|err| {
						CompilerError::HirnApiError(err)
							.to_diagnostic_builder()
							.label(self.get_location(), "Error occured here")
							.build()
					})?;
				let id = local_ctx.scope_map.get(&for_stmt.statement.get_location()).unwrap();
				local_ctx
					.scope
					.insert_api_id(local_ctx.scope.get_variable(*id, &for_stmt.id).unwrap().id, iterator_id);
				use crate::parser::ast::ModuleImplementationStatement::*;
				match for_stmt.statement.as_ref() {
					ModuleImplementationBlockStatement(block) => {
						for statement in &block.statements {
							statement.codegen_pass(ctx, local_ctx, &mut for_scope)?;
						}
					},
					_ => unreachable!(),
				};
			},
			InstantiationStatement(inst) => {
				if ctx.id_table.get_value(&inst.module_name.get_last_module()).as_str() == "register" {
					let r = local_ctx.scope.get_variable(scope_id, &inst.instance_name).unwrap(); //FIXME
					if let VariableKind::ModuleInstance(m) = &r.var.kind {
						if let ModuleInstanceKind::Register(reg) = &m.kind {
							let builder = hirn::design::RegisterBuilder::new(
								api_scope.clone(),
								&ctx.id_table.get_value(&inst.instance_name),
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
									.new_signal(ctx.id_table.get_by_key(&clk_var.var. name).unwrap().as_str())
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
							for stmt in &inst.port_bind {
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
										.label(self.get_location(), "Error occured here")
										.build()
								})?;
							return Ok(());
						}
					}
					unreachable!();
				}
				let name = inst.module_name.get_last_module();
				let module = ctx.modules_declared.get(&name).unwrap();
				let scope = &module.scope;
				let m_handle = module.handle.clone();
				let module_instance = &local_ctx
					.scope
					.get_variable(scope_id, &inst.instance_name)
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
					.new_module(m_handle, &ctx.id_table.get_value(&inst.instance_name).as_str())
					.map_err(|err| {
						CompilerError::HirnApiError(err)
							.to_diagnostic_builder()
							.label(self.get_location(), "Error occured here")
							.build()
					})?;
				debug!("Codegen for generic variables");
				for stmt in &inst.port_bind {
					let var = scope.get_variable(0, &stmt.get_id()).expect("This was checked");
					if !var.var.kind.is_generic() {
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
						.var;
					let var_id: hirn::design::SignalId = var.register(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
						api_scope
							.new_signal(ctx.id_table.get_by_key(&var.name).unwrap().as_str())
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(var.location, "Error occured here")
									.build()
							})?,
					)?;
					builder = builder.bind(&ctx.id_table.get_value(&stmt.get_id()).as_str(), var_id);
					api_scope.assign(var_id.into(), rhs).map_err(|err| {
						CompilerError::HirnApiError(err)
							.to_diagnostic_builder()
							.label(var.location, "Error occured here")
							.build()
					})?;
					debug!("Assigned succesfuly");
				}
				debug!("Codegen for clocks");
				for stmt in &inst.port_bind {
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
						.var;
					let var_id: hirn::design::SignalId = var.register(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
						api_scope
							.new_signal(ctx.id_table.get_by_key(&var.name).unwrap().as_str())
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(var.location, "Error occured here")
									.build()
							})?,
					)?;
					builder = builder.bind(&ctx.id_table.get_value(&stmt.get_id()).as_str(), var_id);
					api_scope.assign(var_id.into(), rhs).map_err(|err| {
						CompilerError::HirnApiError(err)
							.to_diagnostic_builder()
							.label(var.location, "Error occured here")
							.build()
					})?;
					debug!("Assigned succesfuly");
				}
				debug!("Codegen for other variables");
				for stmt in &inst.port_bind {
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
						.var
						.clone();
					let var_id: hirn::design::SignalId = var.register(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&additional_ctx),
						api_scope
							.new_signal(ctx.id_table.get_by_key(&var.name).unwrap().as_str())
							.map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(var.location, "Error occured here")
									.build()
							})?
							.generated(),
					)?;
					builder = builder.bind(&ctx.id_table.get_value(&stmt.get_id()).as_str(), var_id);
					match interface_variable
						.var
						.kind
						.to_signal()
						.expect("This was checked during analysis of a module")
						.direction
					{
						crate::analyzer::Direction::Input(_) => {
							api_scope.assign(var_id.into(), rhs).map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(var.location, "Error occured here")
									.build()
							})?
						},
						crate::analyzer::Direction::Output(_) => {
							api_scope.assign(rhs, var_id.into()).map_err(|err| {
								CompilerError::HirnApiError(err)
									.to_diagnostic_builder()
									.label(var.location, "Error occured here")
									.build()
							})?
						},
						_ => unreachable!(),
					}
					debug!("Assigned succesfuly");
				}
				builder.build().map_err(|err| {
					CompilerError::HirnApiError(err)
						.to_diagnostic_builder()
						.label(self.get_location(), "Error occured here")
						.build()
				})?;
			},
			ModuleImplementationBlockStatement(block) => block.codegen_pass(ctx, local_ctx, api_scope)?,
		};
		Ok(())
	}
}
impl ModuleImplementationBlockStatement {
	pub fn analyze(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
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
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		let mut subscope = api_scope.new_subscope().unwrap();
		for statement in &self.statements {
			statement.codegen_pass(ctx, local_ctx, &mut subscope)?;
		}
		Ok(())
	}
}
impl VariableDefinition {
	pub fn analyze(
		&self,
		already_created: AlreadyCreated,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		scope_id: usize,
	) -> miette::Result<()> {
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
						let entries = expr.get_sensitivity_entry(ctx, &local_ctx.scope, scope_id);
						local_ctx
							.sensitivity_graph
							.add_edges(
								entries,
								crate::analyzer::SensitivityGraphEntry::Signal(
									id,
									direct_initializer.declarator.get_location(),
								),
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
						let entries = expr.get_sensitivity_entry(ctx, &local_ctx.scope, scope_id);
						local_ctx
							.sensitivity_graph
							.add_edges(
								entries,
								crate::analyzer::SensitivityGraphEntry::Signal(
									id,
									direct_initializer.declarator.get_location(),
								),
								expr.get_location(),
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
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		let additional_ctx = AdditionalContext::new(local_ctx.nc_widths.clone(), local_ctx.array_or_bus.clone());
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
impl VariableBlock {
	pub fn analyze(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		mut already_created: AlreadyCreated,
		scope_id: usize,
	) -> miette::Result<()> {
		already_created = analyze_qualifiers(&self.types, already_created, &local_ctx.scope, scope_id, ctx.id_table)?;
		for statement in &self.statements {
			statement.analyze(already_created.clone(), ctx, local_ctx, scope_id)?;
		}
		Ok(())
	}
	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		for statement in &self.statements {
			statement.codegen_pass(ctx, local_ctx, api_scope)?;
		}
		Ok(())
	}
}
impl VariableBlockStatement {
	pub fn analyze(
		&self,
		already_created: AlreadyCreated,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
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
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		match self {
			VariableBlockStatement::VariableBlock(block) => {
				block.codegen_pass(ctx, local_ctx, api_scope)?;
			},
			VariableBlockStatement::VariableDefinition(definition) => {
				definition.codegen_pass(ctx, local_ctx, api_scope)?;
			},
		}
		Ok(())
	}
}
fn create_register(
	inst_stmt: &InstantiationStatement,
	scope_id: usize,
	ctx: &mut GlobalAnalyzerContext,
	local_ctx: &mut LocalAnalyzerContex,
) -> miette::Result<RegisterInstance> {
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
	data_type.sensitivity = SignalSensitivity::Sync(ClockSensitivityList::new().with_clock(clk_var_id, true, data_stmt.unwrap().location()), data_stmt.unwrap().location());
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
	next_type.sensitivity = SignalSensitivity::Comb(ClockSensitivityList::new().with_clock(clk_var_id, true, next_stmt.unwrap().location()), next_stmt.unwrap().location());
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
	en_type.sensitivity =  SignalSensitivity::Comb(ClockSensitivityList::new().with_clock(clk_var_id, true, en_stmt.unwrap().location()), en_stmt.unwrap().location());
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
			en_stmt.unwrap().get_sensitivity_entry(&ctx, &local_ctx.scope, scope_id),
			SensitivityGraphEntry::Signal(en_var_id, en_stmt.unwrap().location()),
			en_stmt.unwrap().location(),
		)
		.map_err(|e| e.build())?;
	local_ctx
		.sensitivity_graph
		.add_edges(
			nreset_stmt
				.unwrap()
				.get_sensitivity_entry(&ctx, &local_ctx.scope, scope_id),
			SensitivityGraphEntry::Signal(nreset_var_id, nreset_stmt.unwrap().location()),
			nreset_stmt.unwrap().location(),
		)
		.map_err(|e| e.build())?;
	local_ctx
		.sensitivity_graph
		.add_edges(
			next_stmt
				.unwrap()
				.get_sensitivity_entry(&ctx, &local_ctx.scope, scope_id),
			SensitivityGraphEntry::Signal(next_var_id, next_stmt.unwrap().location()),
			next_stmt.unwrap().location(),
		)
		.map_err(|e| e.build())?;
	local_ctx
		.sensitivity_graph
		.add_edges(
			clk_stmt
				.unwrap()
				.get_sensitivity_entry(&ctx, &local_ctx.scope, scope_id),
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
pub fn report_qualifier_contradicting_specifier(
	qualifier_location: &SourceSpan,
	specifier_location: &SourceSpan,
	qualifier_name: &str,
	specifier_name: &str,
) -> miette::Result<()> {
	Err(miette::Report::new(
		SemanticError::ContradictingSpecifier
			.to_diagnostic_builder()
			.label(
				*qualifier_location,
				format!(
					"This \"{}\"qualifier contradicts the \"{}\" specifier of this variable",
					qualifier_name, specifier_name
				)
				.as_str(),
			)
			.label(
				*specifier_location,
				format!("This is the \"{}\" specifier of this variable", specifier_name).as_str(),
			)
			.build(),
	))
}
fn report_not_allowed_lhs(location: SourceSpan) -> miette::Result<RegisterInstance> {
	return Err(miette::Report::new(
		SemanticError::ForbiddenExpressionInLhs
			.to_diagnostic_builder()
			.label(
				location,
				"This expression is not allowed in the left hand sight of assignment",
			)
			.build(),
	));
}
fn report_not_allowed_lhs_binding(location: SourceSpan) -> miette::Result<RegisterInstance> {
	return Err(miette::Report::new(
		SemanticError::ForbiddenExpressionInLhs
			.to_diagnostic_builder()
			.label(location, "This expression is not allowed as a binding to output signal")
			.build(),
	));
}
pub fn report_duplicated_qualifier(
	location: &SourceSpan,
	first_occurence: &SourceSpan,
	name: &str,
) -> miette::Result<()> {
	Err(miette::Report::new(
		SemanticError::DuplicateQualifier
			.to_diagnostic_builder()
			.label(
				*location,
				format!("Duplicate occurance of \"{}\"the same type", name).as_str(),
			)
			.label(
				*first_occurence,
				format!("First occurrence of \"{}\" qualifier", name).as_str(),
			)
			.build(),
	))
}
