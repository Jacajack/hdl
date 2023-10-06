use hirn::design::{ScopeHandle, expression::UnaryExpression};
use log::{debug, info};
use serde::de;

use super::{ ModuleDeclared, SemanticError, VariableKind, AlreadyCreated, Variable};
use std::collections::HashMap;

use crate::{
	analyzer::{ModuleImplementationScope, SignalSignedness, Direction, SignalSensitivity, Signal},
	core::IdTable,
	lexer::IdTableKey,
	parser::ast::{
		ModuleImplementation, ModuleImplementationBlockStatement, SourceLocation, TypeQualifier,
		VariableBlock, VariableBlockStatement, VariableDefinition, Root, Scope, analyze_qualifiers, ModuleImplementationStatement,
	},
	ProvidesCompilerDiagnostic, SourceSpan, CompilerError,
};

pub fn combine<'a>(
	id_table: &'a IdTable,
	nc_table: &'a crate::lexer::NumericConstantTable,
	ast: &'a Root,
	mut path_from_root: String,
	present_files: &mut HashMap<String, String>,
) -> miette::Result<(Vec<String>, GlobalAnalyzerContext<'a>, HashMap<IdTableKey, &'a ModuleImplementation>)> {
	use log::info;

	info!("Running combining pass");
	info!("Path from root: {}", path_from_root);

	if path_from_root.as_str() == "" {
		path_from_root = ".".to_string();
	}
	let mut design =  hirn::design::Design::new();
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
					//modules_implemented.remove(&name);
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
		//scope: HashMap::new(),
		design,
	};
	
	Ok((packaged_paths, ctx, modules_implemented))
}
pub struct SemanticalAnalyzer<'a> {
	ctx: GlobalAnalyzerContext<'a>,
	modules_implemented: &'a HashMap< IdTableKey, &'a ModuleImplementation>,
	passes: Vec<for<'b> fn(&mut GlobalAnalyzerContext<'a>, &mut LocalAnalyzerContex, &ModuleImplementation) -> miette::Result<()>>,
}
impl <'a> SemanticalAnalyzer<'a>{
	pub fn new(ctx: GlobalAnalyzerContext<'a>, modules_implemented: &'a HashMap<IdTableKey, &ModuleImplementation>)->Self{
		let mut n = Self { ctx,  modules_implemented, passes: Vec::new() };
		n.passes.push(first_pass);
		n.passes.push(second_pass);
		n.passes.push(codegen_pass);
		n
	}
	pub fn process(&mut self) -> miette::Result<()> {
		for module in self.modules_implemented.values(){
			let mut local_ctx = LocalAnalyzerContex::new(module.id, self.ctx.modules_declared.get(&module.id).unwrap().scope.clone());
			for pass in &self.passes {
				pass(&mut self.ctx, &mut local_ctx, *module)?;
			}
		}
		Ok(())
	}
}
/// This pass collects all variables
pub fn first_pass(ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex, module: &ModuleImplementation) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running initial pass");
	module.first_pass(ctx, local_ctx)?;
	debug!("Initial pass done");
	Ok(())
}
/// This pass checks if all variables have specified sensitivity and width if needed
pub fn second_pass(ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex, module: &ModuleImplementation) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running second pass");
	module.second_pass(ctx, local_ctx)?;
	debug!("Second pass done");
	Ok(())
}
/// This pass invokes HIRN API and creates proper module
pub fn codegen_pass(ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex, module: &ModuleImplementation) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running codegen pass");
	module.codegen_pass(ctx, local_ctx)?;
	debug!("Codegen pass done");
	Ok(())
}
/// Global shared context for semantic analysis
pub struct GlobalAnalyzerContext<'a> {
	pub id_table: &'a IdTable,
	pub nc_table: &'a crate::lexer::NumericConstantTable,
	/// represents all declared modules
	pub modules_declared: HashMap<IdTableKey, ModuleDeclared>,
	/// represents all implemented generic modules
	pub generic_modules: HashMap<IdTableKey, &'a ModuleImplementation>,
	pub design: hirn::design::Design,
}
/// Per module context for semantic analysis
pub struct LocalAnalyzerContex {
	pub scope: ModuleImplementationScope,
	pub dependency_graph: HashMap<usize, Vec<usize>>,
	pub scope_map: HashMap<SourceSpan, usize>,
	pub module_id: IdTableKey,
}
impl LocalAnalyzerContex {
	pub fn new(module_id: IdTableKey, scope: ModuleImplementationScope) -> Self {
		LocalAnalyzerContex {
			scope,
			dependency_graph: HashMap::new(),
			scope_map: HashMap::new(),
			module_id,
		}
	}
}
impl ModuleImplementation {
	// Performs first pass on module implementation
	pub fn first_pass(&self, ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex) -> miette::Result<()> {
		debug!("Analyzing module implementation {}", ctx.id_table.get_by_key(&self.id).unwrap());
		use crate::parser::ast::ModuleImplementationStatement::*;
		match ctx.modules_declared.get(&self.id) {
    		Some(_) => (),
    		None => return Err(miette::Report::new(
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
					.build())),
		}

		let id = 0;
		match &self.statement {
			ModuleImplementationBlockStatement(block) => block.analyze(ctx, local_ctx, id)?,
			_ => unreachable!(),
		};
		debug!("Done analyzing module implementation {}", ctx.id_table.get_by_key(&self.id).unwrap());
		Ok(())
	}
	pub fn second_pass(&self, ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex) -> miette::Result<()> {
		local_ctx.scope.second_pass(ctx)?;
		Ok(())
	}
	
	pub fn codegen_pass(&self, ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex) -> miette::Result<()> {
		debug!("Codegen pass for module implementation {}", ctx.id_table.get_by_key(&self.id).unwrap());
		//let module  = ctx.modules_declared.get(&self.id).unwrap();
		let mut api_scope = ctx.modules_declared.get_mut(&local_ctx.module_id).unwrap().handle.scope();

		//for var in module.scope.signals.values() {
		//	// create variable with API
		//	var.register(ctx, local_ctx, &mut api_scope)?;
		//}

		use crate::parser::ast::ModuleImplementationStatement::*;
		match &self.statement {
			ModuleImplementationBlockStatement(block) => block.codegen_pass(ctx, local_ctx, &mut api_scope)?,
			_ => unreachable!(),
		};
		debug!("Done codegen pass for module implementation {}", ctx.id_table.get_by_key(&self.id).unwrap());
		Ok(())
	}
}
impl ModuleImplementationStatement{
	pub fn first_pass(&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		scope_id: usize,
	) -> miette::Result<()>{
		local_ctx.scope_map.insert(self.get_location(), scope_id);
		use ModuleImplementationStatement::*;
		match self {
			VariableBlock(block) => block.analyze(ctx, local_ctx, AlreadyCreated::new(), scope_id)?,
			VariableDefinition(definition) => {
				definition.analyze(AlreadyCreated::new(), ctx, local_ctx, scope_id)?
			},
			AssignmentStatement(assignment) => {
				let lhs_type = assignment.lhs.evaluate_type(ctx.id_table, ctx.nc_table, scope_id, &mut local_ctx.scope, Signal::new_empty(), true, assignment.location)?;
				info!("Lhs type at the beginning: {:?}",lhs_type);
				let rhs_type = assignment.rhs.evaluate_type(ctx.id_table, ctx.nc_table, scope_id, &mut local_ctx.scope, lhs_type, false, assignment.location)?;
				info!("Rhs type at the end: {:?}",rhs_type);
				info!("Lhs type at the and: {:?}",assignment.lhs.evaluate_type(ctx.id_table, ctx.nc_table, scope_id, &mut local_ctx.scope, rhs_type, true, assignment.location)?);
			},	
			IfElseStatement(conditional) => {
				let condition_type = conditional.condition.evaluate(ctx.nc_table, scope_id, &mut local_ctx.scope)?;
				let if_scope = local_ctx.scope.new_scope(Some(scope_id));
				conditional.if_statement.first_pass(ctx, local_ctx, if_scope)?;
				match &conditional.else_statement{
        			Some(stmt) =>{
						let else_scope = local_ctx.scope.new_scope(Some(scope_id));
						stmt.first_pass(ctx, local_ctx, else_scope)?;
					},
        			None => (),
    			}
			},
			IterationStatement(iteration) => todo!(),
			InstantiationStatement(inst) => {
				let name = inst.module_name.get_last_module();
				ctx.modules_declared.get_mut(&local_ctx.module_id).unwrap().instantiates.push(name.clone());
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
				if !ctx.modules_declared.contains_key(&name){
					return Err(miette::Report::new(
						SemanticError::ModuleNotDeclared
							.to_diagnostic_builder()
							.label(
								inst.module_name.location,
								format!(
									"Module \"{}\" is not declared",
									ctx.id_table.get_by_key(&name).unwrap()
								)
								.as_str(),
							)
							.build(),
					));
				}
				let module = ctx.modules_declared.get(&name).unwrap();
				for stmt in &inst.port_bind{
					// stmt.bind(ctx, local_ctx, scope_id, module.scope.get_scope(0))?;
				}
			},
			ModuleImplementationBlockStatement(block) => {
				let id = local_ctx.scope.new_scope(Some(scope_id));
				block.analyze(ctx, local_ctx, id)?;
			},
		}
		Ok(())
	}
	pub fn codegen_pass(&self, ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle) -> miette::Result<()>
	{
		let scope_id = local_ctx.scope_map.get(&self.get_location()).unwrap().to_owned();
		use ModuleImplementationStatement::*;
		match self {
    		VariableBlock(block) => block.codegen_pass(ctx, local_ctx, api_scope)?,
    		VariableDefinition(definition) => {
				definition.codegen_pass(ctx, local_ctx, api_scope)? 
			},
    		AssignmentStatement(assignment) => {
				let lhs = assignment.lhs.codegen(ctx.nc_table, scope_id, &local_ctx.scope)?;
				let rhs = assignment.rhs.codegen(ctx.nc_table, scope_id, &local_ctx.scope)?;
				api_scope.assign(lhs, rhs).map_err(|err| CompilerError::HirnApiError(err).to_diagnostic())?;
				debug!("Assignment done");
				debug!("Module: {:?}", ctx.modules_declared.get(&local_ctx.module_id).unwrap().handle);
			},
    		IfElseStatement(conditional) => {
				let mut inner_scope = api_scope.if_scope(conditional.condition.codegen(ctx.nc_table, scope_id, &local_ctx.scope)?).unwrap();
				conditional.if_statement.codegen_pass(ctx, local_ctx, &mut inner_scope)?;
				match conditional.else_statement{
					Some(ref else_statement) => {
						let expr = conditional.condition.codegen(ctx.nc_table, scope_id, &local_ctx.scope)?;
						let mut else_scope = api_scope.if_scope(hirn::Expression::Unary(UnaryExpression{
							op: hirn::UnaryOp::LogicalNot,
							operand: Box::new(expr),
						})).unwrap();
						else_statement.codegen_pass(ctx, local_ctx, &mut else_scope)?
					},
					None => (),
				}
			},
    		IterationStatement(_) => todo!(),
    		InstantiationStatement(_) => todo!(),
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
	pub fn codegen_pass(&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle) -> miette::Result<()> {
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
		let mut kind = VariableKind::from_type_declarator(&self.type_declarator, scope_id, already_created, ctx.nc_table, ctx.id_table, &local_ctx.scope)?;
		match &kind{
    		VariableKind::Signal(sig) => {
				if sig.is_direction_specified(){
					return Err(miette::Report::new(
						SemanticError::SignalDirectionSpecified
							.to_diagnostic_builder()
							.label(
								self.location,
								"This signal is specified as interface's signal in module implementation"
							)
							.build(),
					));
				}
			},
    		VariableKind::Generic(_) => (),
			VariableKind::ModuleInstance(_) => (),
		}
		for direct_initializer in &self.initializer_list{
			if let Some(variable) = local_ctx.scope.get_variable_in_scope(scope_id, &direct_initializer.declarator.name) {
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
			for array_declarator in &direct_initializer.declarator.array_declarators{
				let size = array_declarator.evaluate(ctx.nc_table, scope_id, &local_ctx.scope)?.value;
				if size <= num_bigint::BigInt::from(0){
					return Err(miette::Report::new(SemanticError::NegativeBusWidth.to_diagnostic_builder().label(array_declarator.get_location(), "Array size must be positive").build()));
				}
				dimensions.push(size);
			}
			kind.add_dimenstions(dimensions);
			local_ctx.scope.define_variable(scope_id, Variable{
				name: direct_initializer.declarator.name,
				kind: kind.clone(),
				//dimensions,
				location: direct_initializer.declarator.get_location(),
			})?;
			match &direct_initializer.expression{
    			Some(expr) => {
					let lhs = kind.to_signal();
					let rhs = expr.evaluate_type(ctx.id_table, ctx.nc_table, scope_id, &mut local_ctx.scope, lhs, false, direct_initializer.declarator.get_location())?; // FIXME
					// todo finish this assignment
				},
    			None => (),
			}
			debug!("Defined variable {:?} in scope {}", ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap(), scope_id);
		}
		Ok(())
	}
	pub fn codegen_pass(&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle) -> miette::Result<()> {
		let scope_id = local_ctx.scope_map.get(&self.location).unwrap().to_owned();
		for direct_initializer in &self.initializer_list{
			let variable = local_ctx.scope.get_variable_in_scope(scope_id, &direct_initializer.declarator.name).unwrap();
			let api_id = variable.var.register(ctx.id_table, scope_id, &local_ctx.scope, api_scope.new_signal().unwrap())?;
			match &direct_initializer.expression {
    			Some(expr) => api_scope.assign(api_id.into(), expr.codegen(ctx.nc_table, scope_id, &local_ctx.scope)?).unwrap(),
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
	pub fn codegen_pass(&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle
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
	pub fn codegen_pass(&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle) -> miette::Result<()> {
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
