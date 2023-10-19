use super::{AlreadyCreated, ModuleDeclared, SemanticError, Variable, VariableKind, RegisterInstance};
use hirn::design::{ScopeHandle, UnaryExpression};
use log::{debug, info};
use std::collections::HashMap;
use std::io::Write;

use crate::{
	analyzer::{BusWidth, ModuleImplementationScope, Signal, ModuleInstance, semantic_error::InstanceError, GenericVariable, SignalSensitivity, ClockSensitivityList},
	core::*,
	lexer::*,
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
	let mut design = hirn::design::Design::new();
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
			let mut sv_codegen = hirn::codegen::sv::SVCodegen::new(&mut self.ctx.design);
			use hirn::codegen::Codegen;
			let mut output_string = String::new();
			sv_codegen
				.emit_module(
					&mut output_string,
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
	// all passes are performed per module
	debug!("Running codegen pass");
	module.codegen_pass(ctx, local_ctx)?;
	debug!("Codegen pass done");
	Ok(())
}
/// Global shared context for semantic analysis
pub struct GlobalAnalyzerContext<'a> {
	pub id_table: &'a mut IdTable,
	pub nc_table: &'a crate::lexer::NumericConstantTable,
	/// represents all declared modules
	pub modules_declared: HashMap<IdTableKey, ModuleDeclared>,
	/// represents all implemented generic modules
	pub generic_modules: HashMap<IdTableKey, &'a ModuleImplementation>,
	pub design: hirn::design::Design,
}
/// Per module context for semantic analysis
pub struct LocalAnalyzerContex {
	pub are_we_in_conditional: usize,
	pub scope: ModuleImplementationScope,
	pub nc_widths: HashMap<SourceSpan, NumericConstant>,
	pub widths_map: HashMap<SourceSpan, BusWidth>,
	pub scope_map: HashMap<SourceSpan, usize>,
	pub module_id: IdTableKey,
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
		}
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
		local_ctx.scope.second_pass(ctx)?;
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
		use ModuleImplementationStatement::*;
		match self {
			VariableBlock(block) => block.analyze(ctx, local_ctx, AlreadyCreated::new(), scope_id)?,
			VariableDefinition(definition) => definition.analyze(AlreadyCreated::new(), ctx, local_ctx, scope_id)?,
			AssignmentStatement(assignment) => {
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
						Some(val) => {
							assignment.lhs.assign(
								BusWidth::EvaluatedLocated(val, assignment.rhs.get_location()),
								local_ctx,
								scope_id,
							);
						},
						None => assignment.lhs.assign(
							BusWidth::Evaluable(assignment.rhs.get_location()),
							local_ctx,
							scope_id,
						),
					}
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
				info!(
					"Lhs type at the and: {:?}",
					assignment
						.lhs
						.evaluate_type(ctx, scope_id, local_ctx, rhs_type, true, assignment.location)?
				);
			},
			IfElseStatement(conditional) => {
				let condition_type = conditional
					.condition
					.evaluate(ctx.nc_table, scope_id, &mut local_ctx.scope)?;
				let if_scope = local_ctx.scope.new_scope(Some(scope_id));
				local_ctx.are_we_in_conditional += 1;
				conditional.if_statement.first_pass(ctx, local_ctx, if_scope)?;
				match &conditional.else_statement {
					Some(stmt) => {
						let else_scope = local_ctx.scope.new_scope(Some(scope_id));
						stmt.first_pass(ctx, local_ctx, else_scope)?;
					},
					None => (),
				}
				local_ctx.are_we_in_conditional -= 1;
			},
			IterationStatement(iteration) => {
				local_ctx.are_we_in_conditional += 1;
				todo!();
				local_ctx.are_we_in_conditional -= 1;
			},
			InstantiationStatement(inst) => {
				let name = inst.module_name.get_last_module();
				match local_ctx.scope.is_declared(scope_id, &inst.instance_name){
        			Some(location) => return Err(miette::Report::new(
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
					)),
        			None => (),
    			} 
				if ctx.id_table.get_value(&name).as_str() == "register"{
					let v = Variable::new(inst.instance_name,inst.location, VariableKind::ModuleInstance(ModuleInstance { module_name: inst.instance_name, location: inst.location, kind: crate::analyzer::ModuleInstanceKind::Register(create_register(inst, scope_id, ctx, local_ctx)?)}));
					local_ctx.scope.define_variable(scope_id, v )?;
				}
				ctx.modules_declared
					.get_mut(&local_ctx.module_id)
					.unwrap()
					.instantiates
					.push(name.clone());
				if name == local_ctx.module_id {
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
				let module = ctx.modules_declared.get(&name).unwrap();
				let mut scope = module.scope.clone();
				let mut module_instance = ModuleInstance::new(name, inst.location);
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
						.map_err(|err| {
							err.to_diagnostic_builder()
								.label(
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
					use crate::parser::ast::PortBindStatement::*;
					match &stmt {
						OnlyId(id) => {
							debug!("Only id");
							let mut local_sig = local_ctx
								.scope
								.get_var(scope_id, &id.id)
								.map_err(|err| {
									err.to_diagnostic_builder()
										.label(
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
									scope.redeclare_variable(interface_variable);
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
							scope.redeclare_variable(interface_variable);
						},
						IdWithDeclaration(id_decl) => {
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
				debug!("Binding non generic variables!");
				for stmt in &inst.port_bind {
					let mut interface_variable = scope
						.get_var(0, &stmt.get_id())
						.expect("This was checked in a previous loop")
						.clone();
					if interface_variable.var.kind.is_generic() {
						continue;
					}
					debug!("Interface variable is {:?}", interface_variable.var.kind);
					//interface_variable.var.kind.evaluate_bus_width(&scope, ctx.id_table, ctx.nc_table)?;
					use crate::parser::ast::PortBindStatement::*;
					match &stmt {
						OnlyId(id) => {
							debug!("Only id");
							let mut local_sig = local_ctx
								.scope
								.get_var(scope_id, &id.id)
								.map_err(|err| {
									err.to_diagnostic_builder()
										.label(
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
								(Signal(sig1), Signal(sig2)) => {
									sig2.evaluate_as_lhs(true, ctx, sig1.clone(), stmt.location())?;
									// sig 2 change width to the ones known in local scope
									sig1.evaluate_as_lhs(false, ctx, sig2.clone(), stmt.location())?;
									//local_ctx.scope.evaluated_expressions.insert(sig2.width().unwrap().get_location().unwrap(), scope.evaluated_expressions.get(&sig2.width().unwrap().get_location().unwrap()).unwrap().clone());
									local_ctx.scope.redeclare_variable(local_sig);
								},
								(Generic(_), Signal(_)) => {
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
								(..) => unreachable!(),
							}
						},
						IdWithExpression(id_expr) => {
							debug!("Id with expression");

							let new_sig = id_expr.expression.evaluate_type(
								&ctx,
								scope_id,
								local_ctx,
								interface_variable.var.kind.to_signal(),
								false,
								id_expr.location,
							)?;
						},
						IdWithDeclaration(id_decl) => {
							debug!("Id with declaration");

							let new_war = VariableKind::from_type_declarator(
								&id_decl.declaration.type_declarator,
								scope_id,
								AlreadyCreated::new(),
								ctx.nc_table,
								ctx.id_table,
								&mut local_ctx.scope,
							)?;
							let mut sig = new_war.to_signal();
							sig.evaluate_as_lhs(false, &ctx, interface_variable.var.kind.to_signal(), id_decl.location)?
						},
					}
				}
				local_ctx.scope.define_variable(
					scope_id,
					Variable::new(
						inst.instance_name,
						inst.location,
						VariableKind::ModuleInstance(module_instance),
					),
				)?;
			},
			ModuleImplementationBlockStatement(block) => {
				let id = local_ctx.scope.new_scope(Some(scope_id));
				block.analyze(ctx, local_ctx, id)?;
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
		let scope_id = local_ctx.scope_map.get(&self.get_location()).unwrap().to_owned();
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
					Some(&local_ctx.nc_widths),
				)?;
				let rhs = assignment.rhs.codegen(
					ctx.nc_table,
					ctx.id_table,
					scope_id,
					&local_ctx.scope,
					Some(&local_ctx.nc_widths),
				)?;
				use crate::parser::ast::AssignmentOpcode::*;
				match assignment.assignment_opcode {
					Equal => api_scope
						.assign(lhs, rhs)
						.map_err(|err| CompilerError::HirnApiError(err).to_diagnostic())?,
					PlusEqual => todo!(), // does it make sense?
					AndEqual => api_scope
						.assign(lhs.clone(), lhs & rhs)
						.map_err(|err| CompilerError::HirnApiError(err).to_diagnostic())?,
					XorEqual => api_scope
						.assign(lhs.clone(), lhs ^ rhs)
						.map_err(|err| CompilerError::HirnApiError(err).to_diagnostic())?,
					OrEqual => api_scope
						.assign(lhs.clone(), lhs | rhs)
						.map_err(|err| CompilerError::HirnApiError(err).to_diagnostic())?,
				};

				debug!("Assignment done");
			},
			IfElseStatement(conditional) => {
				let mut inner_scope = api_scope
					.if_scope(conditional.condition.codegen(
						ctx.nc_table,
						ctx.id_table,
						scope_id,
						&local_ctx.scope,
						Some(&local_ctx.nc_widths),
					)?)
					.unwrap();
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
							Some(&local_ctx.nc_widths),
						)?;
						let mut else_scope = api_scope
							.if_scope(hirn::design::Expression::Unary(UnaryExpression {
								op: hirn::design::UnaryOp::LogicalNot,
								operand: Box::new(expr),
							}))
							.unwrap();
						else_statement.codegen_pass(ctx, local_ctx, &mut else_scope)?
					},
					None => (),
				}
			},
			IterationStatement(_) => todo!(),
			InstantiationStatement(_) => (), //FIXME
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
					if spec_kind.is_generic(){
						let rhs_val = expr.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)?;
						if let VariableKind::Generic(GenericVariable{value, ..}) = &mut spec_kind {
							value.replace(BusWidth::Evaluated(rhs_val.unwrap()));
						} else {
							unreachable!()
						}
					}
					else {
						let mut lhs = spec_kind.to_signal();
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
					}
				},
				None => (),
			}
			local_ctx.scope.define_variable(
				scope_id,
				Variable {
					name: direct_initializer.declarator.name,
					kind: spec_kind,
					location: direct_initializer.declarator.get_location(),
				},
			)?;
			debug!(
				"Defined variable {:?} in scope {}",
				ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap(),
				scope_id
			);
		}
		Ok(())
	}
	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
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
				Some(&local_ctx.nc_widths),
				api_scope
					.new_signal(ctx.id_table.get_by_key(&variable.var.name).unwrap().as_str())
					.unwrap(),
			)?;
			match &direct_initializer.expression {
				Some(expr) => api_scope
					.assign(
						api_id.into(),
						expr.codegen(
							ctx.nc_table,
							ctx.id_table,
							scope_id,
							&local_ctx.scope,
							Some(&local_ctx.nc_widths),
						)?,
					)
					.unwrap(),
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
fn create_register(inst_stmt: &InstantiationStatement,scope_id: usize, ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex) ->miette::Result<RegisterInstance>{
	if inst_stmt.port_bind.len() != 5 {
		return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
		.label(inst_stmt.location, "Register must have 5 arguments")
		.build()))
	}
	let mut en_stmt: Option<&PortBindStatement> = None;
	let mut next_stmt: Option<&PortBindStatement> = None;
	let mut clk_stmt: Option<&PortBindStatement> = None;
	let mut nreset_stmt: Option<&PortBindStatement> = None;
	let mut data_stmt: Option<&PortBindStatement> = None;
	for stmt in &inst_stmt.port_bind{
		match ctx.id_table.get_value(&stmt.get_id()).as_str(){
			"en" => match en_stmt{
				Some(stmt1) => return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
				.label(stmt.location(), "This argument is already defined")
				.label(stmt1.location(), "En signal is already defined here")
				.build())),
				None =>en_stmt.replace(stmt),
			},
			"next" => match next_stmt{
				Some(stmt1) => return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
				.label(stmt.location(), "This argument is already defined")
				.label(stmt1.location(), "Next signal is already defined here")
				.build())),
				None =>next_stmt.replace(stmt),
			},
			"clk" => match clk_stmt{
				Some(stmt1) => return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
				.label(stmt.location(), "This argument is already defined")
				.label(stmt1.location(), "Clk signal is already defined here")
				.build())),
				None =>clk_stmt.replace(stmt),
			},
			"nreset" => match nreset_stmt{
				Some(stmt1) => return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
				.label(stmt.location(), "This argument is already defined")
				.label(stmt1.location(), "NReset signal is already defined here")
				.build())),
				None =>nreset_stmt.replace(stmt),
			},
			"data" => match data_stmt{
				Some(stmt1) => return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
				.label(stmt.location(), "This argument is already defined")
				.label(stmt1.location(), "Data signal is already defined here")
				.build())),
				None =>data_stmt.replace(stmt),
			},
			_ => return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
			.label(stmt.location(), "This is not a valid argument for register")
			.build()))
		};
	}
	use crate::parser::ast::PortBindStatement::*;
	let clk_signal = Signal{
    	signal_type: crate::analyzer::SignalType::Wire(clk_stmt.unwrap().location()),
    	dimensions: vec![],
    	sensitivity: crate::analyzer::SignalSensitivity::Clock(clk_stmt.unwrap().location()),
    	direction: crate::analyzer::Direction::Input(clk_stmt.unwrap().location()),
	};
	let clk_type = match clk_stmt.unwrap(){
    	OnlyId(id) => todo!(),
    	IdWithExpression(expr) => expr.expression.evaluate_type(ctx, scope_id, local_ctx, clk_signal.clone(), false, clk_stmt.unwrap().location())?,
    	IdWithDeclaration(_) => todo!(),
	};
	debug!("Clk type is {:?}", clk_type);
	if clk_type.is_array(){
		return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
		.label(clk_stmt.unwrap().location(), "Clock cannot be array")
		.build()))
	}
	if let SignalSensitivity::Clock(_) = &clk_type.sensitivity{}
	else {
		return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
		.label(clk_stmt.unwrap().location(), "Clk signal must be marked as clock")
		.build()))
	}
	let clk_name = ctx.id_table.insert_or_get("clk_in2137");
	let clk_var = Variable::new(clk_name, clk_stmt.unwrap().location(), VariableKind::Signal(clk_type.clone()));
	let data_signal = Signal{
		signal_type: crate::analyzer::SignalType::Auto(data_stmt.unwrap().location()),
		dimensions: vec![],
		sensitivity: crate::analyzer::SignalSensitivity::Comb(ClockSensitivityList::new().with_clock(clk_name, true, clk_stmt.unwrap().location()), clk_stmt.unwrap().location()), //FIXME
		direction: crate::analyzer::Direction::Input(data_stmt.unwrap().location()),
	};
	let mut data_type = match data_stmt.unwrap(){
		OnlyId(id) => todo!(),
		IdWithExpression(expr) => expr.expression.evaluate_type(ctx, scope_id, local_ctx, data_signal.clone(), false, data_stmt.unwrap().location())?,
		IdWithDeclaration(_) => todo!(),
	};
	debug!("Data type is {:?}", data_type);
	if data_type.is_array(){
		return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
		.label(data_stmt.unwrap().location(), "Clock cannot be array")
		.build()))
	}
	if !data_type.sensitivity.is_not_worse_than(&clk_type.sensitivity) {
		return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
		.label(data_stmt.unwrap().location(), "Data signal must be synchronized with clock")
		.build()))
	}
	let next_signal = Signal{
		signal_type: crate::analyzer::SignalType::Auto(data_stmt.unwrap().location()),
		dimensions: vec![],
		sensitivity: crate::analyzer::SignalSensitivity::NoSensitivity,
		direction: crate::analyzer::Direction::Input(data_stmt.unwrap().location()),
	};
	let mut next_type = match next_stmt.unwrap(){
		OnlyId(id) => todo!(),
		IdWithExpression(expr) => expr.expression.evaluate_type(ctx, scope_id, local_ctx, data_type.clone(), false, data_stmt.unwrap().location())?,
		IdWithDeclaration(_) => todo!(),
	};
	debug!("Next type is {:?}", next_type);
	if next_type.is_array(){
		return Err(miette::Report::new(InstanceError::ArgumentsMismatch.to_diagnostic_builder()
		.label(clk_stmt.unwrap().location(), "Clock cannot be array")
		.build()))
	}
	//data_type.sensitivity.can_drive(&clk_signal.sensitivity, data_stmt.unwrap().location(), ctx)?;
	todo!()
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
