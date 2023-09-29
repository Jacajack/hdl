use hirn::design::{ScopeHandle, SignalDirection};
use log::debug;

use super::{CombinedQualifiers, ModuleDeclared, SemanticError};
use std::collections::HashMap;

use crate::{
	analyzer::ModuleImplementationScope,
	core::IdTable,
	lexer::IdTableKey,
	parser::ast::{
		ModuleImplementation, ModuleImplementationBlockStatement, SourceLocation, TypeQualifier, TypeSpecifier,
		VariableBlock, VariableBlockStatement, VariableDefinition, Root,
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
				debug!(
					"Found module declaration for {:?}",
					id_table.get_by_key(&declaration.id).unwrap()
				);
				
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
		scope_map: HashMap::new(),
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
		//n.passes.push(initial_pass);
		n.passes.push(codegen_pass);
		n
	}
	pub fn process(&mut self) -> miette::Result<()> {
		for module in self.modules_implemented.values(){
			let mut local_ctx = LocalAnalyzerContex::new(&mut self.ctx.design, self.ctx.id_table.get_by_key(&module.id).unwrap().to_string(), module.id);
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
	pub scope_map: HashMap<SourceSpan, usize>,
	pub design: hirn::design::Design,
}
/// Per module context for semantic analysis
pub struct LocalAnalyzerContex {
	pub scope_map: ModuleImplementationScope,
	pub module_handle: hirn::design::ModuleHandle,
	pub module_id: IdTableKey,
}
impl LocalAnalyzerContex {
	pub fn new(design_handle: &mut hirn::design::Design, module_name: String, module_id: IdTableKey) -> Self {
		LocalAnalyzerContex {
			scope_map: ModuleImplementationScope::new(),
			module_handle: design_handle.new_module(&module_name).unwrap(),
			module_id,
		}
	}
}
impl ModuleImplementation {
	// Performs first pass on module implementation
	pub fn analyze(&self, ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex) -> miette::Result<()> {
		debug!("Analyzing module implementation {}", ctx.id_table.get_by_key(&self.id).unwrap());
		use crate::parser::ast::ModuleImplementationStatement::*;
		let module  = ctx.modules_declared.get(&self.id).unwrap();
		//for var_id in module.scope.variables.keys() {
		//	let (var, loc) = module.scope.variables.get(var_id).unwrap();
		//	// create variable with API
		//}
		let id = local_ctx.scope_map.new_scope(None);
		match &self.statement {
			ModuleImplementationBlockStatement(block) => block.analyze(ctx, local_ctx, id)?,
			_ => unreachable!(),
		};
		debug!("Done analyzing module implementation {}", ctx.id_table.get_by_key(&self.id).unwrap());
		Ok(())
	}
	pub fn codegen_pass(&self, ctx: &mut GlobalAnalyzerContext, local_ctx: &mut LocalAnalyzerContex) -> miette::Result<()> {
		debug!("Codegen pass for module implementation {}", ctx.id_table.get_by_key(&self.id).unwrap());
		let module  = ctx.modules_declared.get(&self.id).unwrap();
		let mut api_scope: ScopeHandle = local_ctx.module_handle.scope();

		//for var in module.scope.signals.values() {
		//	// create variable with API
		//	var.register(ctx, local_ctx, &mut api_scope)?;
		//}

		use crate::parser::ast::ModuleImplementationStatement::*;
		match &self.statement {
			ModuleImplementationBlockStatement(block) => (),
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
		ctx.scope_map.insert(self.location, scope_id);
		use crate::parser::ast::ModuleImplementationStatement::*;
		for statement in &self.statements {
			match statement {
				VariableBlock(block) => block.analyze(ctx, local_ctx, CombinedQualifiers::new(), scope_id)?,
				VariableDefinition(definition) => {
					definition.analyze(CombinedQualifiers::new(), ctx, local_ctx, scope_id)?
				},
				AssignmentStatement(_) => todo!(),
				IfElseStatement(conditional) => {
					
				},
				IterationStatement(_) => todo!(),
				InstantiationStatement(_) => todo!(),
				ModuleImplementationBlockStatement(block) => {
					let id = local_ctx.scope_map.new_scope(Some(scope_id));
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
		use crate::parser::ast::ModuleImplementationStatement::*;
		for statement in &self.statements {
			//match statement{
			//	VariableBlock(block) => block.codegen_pass(ctx, local_ctx, api_scope)?,
			//	VariableDefinition(definition) => {
			//		definition.codegen_pass(ctx, local_ctx, api_scope)?
			//	},
			//	AssignmentStatement(_) => todo!(),
			//	IfElseStatement(conditional) => {
					
			//	},
			//	IterationStatement(_) => todo!(),
			//	InstantiationStatement(inst) => {
			//		api_scope.new_module(module)
			//	},
			//	ModuleImplementationBlockStatement(block) => {
			//		block.codegen_pass(ctx, local_ctx, api_scope)?;
			//	},
			//}
		}
		Ok(())
	}
}
impl VariableDefinition {
	pub fn analyze(
		&self,
		mut already_combined: CombinedQualifiers,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		scope_id: usize,
	) -> miette::Result<()> {
		already_combined = analyze_qualifiers(&self.type_declarator.qualifiers, already_combined)?;
		analyze_specifier_implementation(&self.type_declarator.specifier, &already_combined)?;
		for direct_initializer in &self.initializer_list {
			if let Some(variable) = local_ctx.scope_map.get_variable_in_scope(scope_id, &direct_initializer.declarator.name) {
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
							variable.location,
							format!(
								"Here variable \"{}\" was declared before.",
								ctx.id_table.get_by_key(&direct_initializer.declarator.name).unwrap()
							)
							.as_str(),
						)
						.build(),
				));
			}
		}
		Ok(())
	}
}
impl VariableBlock {
	pub fn analyze(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		mut already_combined: CombinedQualifiers,
		scope_id: usize,
	) -> miette::Result<()> {
		already_combined = analyze_qualifiers(&self.types, already_combined)?;
		for statement in &self.statements {
			statement.analyze(already_combined.clone(), ctx, local_ctx, scope_id)?;
		}
		Ok(())
	}
}
impl VariableBlockStatement {
	pub fn analyze(
		&self,
		already_combined: CombinedQualifiers,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContex,
		scope_id: usize,
	) -> miette::Result<()> {
		match self {
			VariableBlockStatement::VariableBlock(block) => {
				block.analyze(ctx, local_ctx, already_combined, scope_id)?;
			},
			VariableBlockStatement::VariableDefinition(definition) => {
				definition.analyze(already_combined, ctx, local_ctx, scope_id)?;
			},
		}
		Ok(())
	}
}

pub fn analyze_qualifiers(
	type_qualifiers: &Vec<TypeQualifier>,
	mut already_combined: CombinedQualifiers,
) -> miette::Result<CombinedQualifiers> {
	for q in type_qualifiers {
		use TypeQualifier::*;
		match q {
			Signed { location } => {
				if let Some(prev) = &already_combined.signed {
					report_duplicated_qualifier(location, prev, "signed")?;
				}
				already_combined.signed = Some(*location);
			},
			Unsigned { location } => {
				if let Some(prev) = &already_combined.unsigned {
					report_duplicated_qualifier(location, prev, "unsigned")?;
				}
				already_combined.unsigned = Some(*location);
			},
			Const { location } => {
				if let Some(prev) = &already_combined.constant {
					report_duplicated_qualifier(location, prev, "const")?;
				}
				already_combined.constant = Some(*location);
			},
			Clock { location } => {
				if let Some(prev) = &already_combined.clock {
					report_duplicated_qualifier(location, prev, "clock")?;
				}
				already_combined.clock = Some(*location);
			},
			TypeQualifier::Comb(comb) => {
				if let Some(prev) = &already_combined.comb {
					report_duplicated_qualifier(&comb.location, &prev.1, "comb")?;
				}
				already_combined.comb = Some((comb.expressions.clone(), comb.location));
			},
			TypeQualifier::Sync(sync) => {
				if let Some(prev) = &already_combined.synchronous {
					report_duplicated_qualifier(&sync.location, &prev.1, "sync")?;
				}
				already_combined.synchronous = Some((sync.expressions.clone(), sync.location));
			},
			Input { location } => {
				if let Some(prev) = &already_combined.input {
					report_duplicated_qualifier(location, prev, "input")?;
				}
				already_combined.input = Some(*location);
			},
			Output { location } => {
				if let Some(prev) = &already_combined.output {
					report_duplicated_qualifier(location, prev, "output")?;
				}
				already_combined.output = Some(*location);
			},
			Async { location } => {
				if let Some(prev) = &already_combined.asynchronous {
					report_duplicated_qualifier(location, prev, "async")?;
				}
				already_combined.asynchronous = Some(*location);
			},
			Tristate { location: _ } => (),
		};
	}
	already_combined.check_for_contradicting()?;
	Ok(already_combined)
}
pub fn analyze_specifier(type_specifier: &TypeSpecifier, already_combined: &CombinedQualifiers) -> miette::Result<()> {
	match type_specifier {
		TypeSpecifier::Auto { location } => {
			return Err(miette::Report::new(
				SemanticError::AutoSpecifierInDeclaration
					.to_diagnostic_builder()
					.label(*location, "This specifier is not allowed in a declaration")
					.build(),
			))
		},
		TypeSpecifier::Wire { location } => {
			if let Some(location2) = &already_combined.signed {
				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
			}
			else if let Some(location2) = &already_combined.unsigned {
				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
			}
			Ok(())
		},
		_ => Ok(()),
	}
}
pub fn analyze_specifier_implementation(
	type_specifier: &TypeSpecifier,
	already_combined: &CombinedQualifiers,
) -> miette::Result<()> {
	match type_specifier {
		TypeSpecifier::Wire { location } => {
			if let Some(location2) = &already_combined.signed {
				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
			}
			else if let Some(location2) = &already_combined.unsigned {
				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
			}
		},
		_ => (),
	};
	Ok(())
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
