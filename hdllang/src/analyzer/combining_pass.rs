use hirn::design::ScopeHandle;
use log::debug;

use super::{ ModuleDeclared, SemanticError, VariableKind, AlreadyCreated, Variable};
use std::collections::HashMap;

use crate::{
	analyzer::{ModuleImplementationScope, SignalSignedness, Direction, SignalSensitivity},
	core::IdTable,
	lexer::IdTableKey,
	parser::ast::{
		ModuleImplementation, ModuleImplementationBlockStatement, SourceLocation, TypeQualifier,
		VariableBlock, VariableBlockStatement, VariableDefinition, Root, Scope,
	},
	ProvidesCompilerDiagnostic, SourceSpan,
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
		n.passes.push(initial_pass);
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
pub fn codegen_pass(ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex, module: &ModuleImplementation) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running codegen pass");
	module.codegen_pass(ctx, local_ctx)?;
	debug!("Codegen pass done");
	Ok(())
}
pub fn initial_pass(ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex, module: &ModuleImplementation) -> miette::Result<()> {
	// all passes are performed per module
	debug!("Running initial pass");
	module.analyze(ctx, local_ctx)?;
	debug!("Initial pass done");
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
	/// this should be transferred to local context
	//pub scope: HashMap<SourceSpan, usize>,
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
	pub fn analyze(&self, ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex) -> miette::Result<()> {
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
		let module  = ctx.modules_declared.get(&self.id).unwrap();
		//for var_id in module.scope.variables.keys() {
		//	let (var, loc) = module.scope.variables.get(var_id).unwrap();
		//	// create variable with API
		//}
		let id = 0;
		match &self.statement {
			ModuleImplementationBlockStatement(block) => block.analyze(ctx, local_ctx, id)?,
			_ => unreachable!(),
		};
		debug!("Done analyzing module implementation {}", ctx.id_table.get_by_key(&self.id).unwrap());
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
impl ModuleImplementationBlockStatement {
	pub fn analyze(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		scope_id: usize,
	) -> miette::Result<()> {
		local_ctx.scope_map.insert(self.location, scope_id);
		use crate::parser::ast::ModuleImplementationStatement::*;
		for statement in &self.statements {
			match statement {
				VariableBlock(block) => block.analyze(ctx, local_ctx, AlreadyCreated::new(), scope_id)?,
				VariableDefinition(definition) => {
					definition.analyze(AlreadyCreated::new(), ctx, local_ctx, scope_id)?
				},
				AssignmentStatement(assignment) => todo!(),
				IfElseStatement(conditional) => {
					
				},
				IterationStatement(iteration) => todo!(),
				InstantiationStatement(inst) => {
					let name = inst.module_name.get_last_module();
					ctx.modules_declared.get_mut(&local_ctx.module_id).unwrap().instatiaed.push(name.clone());
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
		}
		Ok(())
	}
	pub fn codegen_pass(&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle) -> miette::Result<()> {
		let scope_id = local_ctx.scope_map.get(&self.location).unwrap().to_owned();
		use crate::parser::ast::ModuleImplementationStatement::*;
		for statement in &self.statements {
			match statement{
				VariableBlock(block) => block.codegen_pass(ctx, local_ctx, api_scope)?,
				VariableDefinition(definition) => {
					definition.codegen_pass(ctx, local_ctx, api_scope)? 
				},
			//	AssignmentStatement(_) => todo!(),
				//IfElseStatement(conditional) => {
				//	let inner_scope = api_scope.if_scope(conditional.condition).unwrap();
				//},
			//	IterationStatement(_) => todo!(),
				InstantiationStatement(inst) => {
					let mut  builder = api_scope.new_module(ctx.modules_declared.get(&inst.module_name.get_last_module()).unwrap().handle.to_owned()).unwrap();
					for port_bind in &inst.port_bind{
						use crate::parser::ast::PortBindStatement::*;
						match  port_bind{
        					OnlyId(only) => builder = builder.bind( ctx.id_table.get_by_key(&only.id).unwrap().as_str(), local_ctx.scope.get_api_id(only.id).unwrap().into()),
        					IdWithExpression(_) => todo!(),
        					IdWithDeclaration(_) => todo!(),
    					}
					}
				},
				ModuleImplementationBlockStatement(block) => {
					block.codegen_pass(ctx, local_ctx, &mut api_scope.new_subscope().unwrap())?;
				}
			_ => (),
			}
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
		let kind = VariableKind::from_type_declarator(&self.type_declarator, scope_id, already_created, ctx.nc_table, ctx.id_table, &local_ctx.scope)?;
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
				let size = array_declarator.evaluate_in_declaration(ctx.nc_table)?.value;
				if size <= num_bigint::BigInt::from(0){
					return Err(miette::Report::new(SemanticError::NegativeBusWidth.to_diagnostic_builder().label(array_declarator.get_location(), "Array size must be positive").build()));
				}
				dimensions.push(array_declarator.evaluate_in_declaration(ctx.nc_table)?.value);
			}
			local_ctx.scope.define_variable(scope_id, Variable{
				name: direct_initializer.declarator.name,
				kind: kind.clone(),
				dimensions,
				location: direct_initializer.declarator.get_location(),
			})?;
			debug!("Defined variable {:?} in scope {}", ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap(), scope_id);
		}
		//already_combined = analyze_qualifiers(&self.type_declarator.qualifiers, already_combined)?;
		//analyze_specifier_implementation(&self.type_declarator.specifier, &already_combined)?;
		//for direct_initializer in &self.initializer_list {
		//	if let Some(variable) = local_ctx.scope.get_variable_in_scope(scope_id, &direct_initializer.declarator.name) {
		//		return Err(miette::Report::new(
		//			SemanticError::DuplicateVariableDeclaration
		//				.to_diagnostic_builder()
		//				.label(
		//					direct_initializer.declarator.get_location(),
		//					format!(
		//						"Variable with name \"{}\" declared here, was already declared before.",
		//						ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap()
		//					)
		//					.as_str(),
		//				)
		//				.label(
		//					variable.var.location,
		//					format!(
		//						"Here variable \"{}\" was declared before.",
		//						ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap()
		//					)
		//					.as_str(),
		//				)
		//				.build(),
		//		));
		//	}
		//}
		Ok(())
	}
	pub fn codegen_pass(&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		api_scope: &mut ScopeHandle) -> miette::Result<()> {
			let scope_id = local_ctx.scope_map.get(&self.location).unwrap().to_owned();
		for direct_initializer in &self.initializer_list{
			let variable = local_ctx.scope.get_variable_in_scope(scope_id, &direct_initializer.declarator.name).unwrap();
			match &variable.var.kind{
    			VariableKind::Signal(sig) => {
					if ! sig.is_sensititivity_specified(){
						return Err(miette::Report::new(
							SemanticError::MissingSensitivityQualifier
								.to_diagnostic_builder()
								.label(
									self.location,
									"Signal must be either const, clock, comb, sync or async"
								)
								.build(),
						));
					}
					if ! sig.is_signedness_specified() {
						return Err(miette::Report::new(
							SemanticError::MissingSignednessQualifier
								.to_diagnostic_builder()
								.label(
									self.location,
									"Bus signal must be either signed or unsigned"
								)
								.build(),
						));
					}
				},
    			VariableKind::Generic(_) => (),
			}
			variable.var.register(ctx.id_table, &local_ctx.scope, api_scope.new_signal().unwrap())?;
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
		for qualifier in &self.types{
			use TypeQualifier::*;
			match qualifier{
				Signed { location } => already_created.add_signedness(SignalSignedness::Signed(*location))?,
				Unsigned { location } => already_created.add_signedness(SignalSignedness::Unsigned(*location))?,
				Tristate { location } => already_created.add_direction(Direction::Tristate(*location))?,
				Const { location } => already_created.add_sensitivity(SignalSensitivity::Const(*location))?,
				Clock { location } => already_created.add_sensitivity(SignalSensitivity::Clock(*location))?,
				Comb(_) => todo!(),
				Sync(_) => todo!(),
				Input { location } => already_created.add_direction(Direction::Input(*location))?,
				Output { location } => already_created.add_direction(Direction::Output(*location))?,
				Async { location } => already_created.add_sensitivity(SignalSensitivity::Async(*location))?,
			}
		}
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

//pub fn analyze_qualifiers(
//	type_qualifiers: &Vec<TypeQualifier>,
//	mut already_combined: CombinedQualifiers,
//) -> miette::Result<CombinedQualifiers> {
//	for q in type_qualifiers {
//		use TypeQualifier::*;
//		match q {
//			Signed { location } => {
//				if let Some(prev) = &already_combined.signed {
//					report_duplicated_qualifier(location, prev, "signed")?;
//				}
//				already_combined.signed = Some(*location);
//			},
//			Unsigned { location } => {
//				if let Some(prev) = &already_combined.unsigned {
//					report_duplicated_qualifier(location, prev, "unsigned")?;
//				}
//				already_combined.unsigned = Some(*location);
//			},
//			Const { location } => {
//				if let Some(prev) = &already_combined.constant {
//					report_duplicated_qualifier(location, prev, "const")?;
//				}
//				already_combined.constant = Some(*location);
//			},
//			Clock { location } => {
//				if let Some(prev) = &already_combined.clock {
//					report_duplicated_qualifier(location, prev, "clock")?;
//				}
//				already_combined.clock = Some(*location);
//			},
//			TypeQualifier::Comb(comb) => {
//				if let Some(prev) = &already_combined.comb {
//					report_duplicated_qualifier(&comb.location, &prev.1, "comb")?;
//				}
//				already_combined.comb = Some((comb.expressions.clone(), comb.location));
//			},
//			TypeQualifier::Sync(sync) => {
//				if let Some(prev) = &already_combined.synchronous {
//					report_duplicated_qualifier(&sync.location, &prev.1, "sync")?;
//				}
//				already_combined.synchronous = Some((sync.expressions.clone(), sync.location));
//			},
//			Input { location } => {
//				if let Some(prev) = &already_combined.input {
//					report_duplicated_qualifier(location, prev, "input")?;
//				}
//				already_combined.input = Some(*location);
//			},
//			Output { location } => {
//				if let Some(prev) = &already_combined.output {
//					report_duplicated_qualifier(location, prev, "output")?;
//				}
//				already_combined.output = Some(*location);
//			},
//			Async { location } => {
//				if let Some(prev) = &already_combined.asynchronous {
//					report_duplicated_qualifier(location, prev, "async")?;
//				}
//				already_combined.asynchronous = Some(*location);
//			},
//			Tristate { location: _ } => (),
//		};
//	}
//	already_combined.check_for_contradicting()?;
//	Ok(already_combined)
//}
//pub fn analyze_specifier(type_specifier: &TypeSpecifier, already_combined: &CombinedQualifiers) -> miette::Result<()> {
//	match type_specifier {
//		TypeSpecifier::Auto { location } => {
//			return Err(miette::Report::new(
//				SemanticError::AutoSpecifierInDeclaration
//					.to_diagnostic_builder()
//					.label(*location, "This specifier is not allowed in a declaration")
//					.build(),
//			))
//		},
//		TypeSpecifier::Wire { location } => {
//			if let Some(location2) = &already_combined.signed {
//				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
//			}
//			else if let Some(location2) = &already_combined.unsigned {
//				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
//			}
//			Ok(())
//		},
//		_ => Ok(()),
//	}
//}
//pub fn analyze_specifier_implementation(
//	type_specifier: &TypeSpecifier,
//	already_combined: &CombinedQualifiers,
//) -> miette::Result<()> {
//	match type_specifier {
//		TypeSpecifier::Wire { location } => {
//			if let Some(location2) = &already_combined.signed {
//				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
//			}
//			else if let Some(location2) = &already_combined.unsigned {
//				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
//			}
//		},
//		_ => (),
//	};
//	Ok(())
//}
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
