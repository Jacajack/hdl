use super::{analyzer_pass::preamble::*, CombinedQualifiers, ModuleDeclared, SemanticError};
use std::collections::HashMap;

use crate::{
	analyzer::ModuleImplementationScope,
	core::IdTable,
	lexer::IdTableKey,
	parser::ast::{
		ModuleImplementation, ModuleImplementationBlockStatement, SourceLocation, TypeQualifier, TypeSpecifier,
		VariableBlock, VariableBlockStatement, VariableDefinition,
	},
	ProvidesCompilerDiagnostic, SourceSpan,
};

pub fn combine(
	id_table: &IdTable,
	nc_table: &crate::lexer::NumericConstantTable,
	ast: &Root,
	mut path_from_root: String,
	present_files: &mut HashMap<String, String>,
) -> miette::Result<Vec<String>> {
	use log::{debug, info};

	info!("Running combining pass");
	info!("Path from root: {}", path_from_root);

	if path_from_root.as_str() == "" {
		path_from_root = ".".to_string();
	}

	let mut packaged_paths: Vec<String> = Vec::new();
	let mut modules_declared: HashMap<IdTableKey, (ModuleDeclared, SourceSpan)> = HashMap::new();
	let mut modules_implemented: HashMap<IdTableKey, &ModuleImplementation> = HashMap::new();
	let mut generic_modules: HashMap<&IdTableKey, &ModuleImplementation> = HashMap::new();

	for def in &ast.definitions {
		use crate::parser::ast::TopDefinition::*;
		match def {
			ModuleDeclaration(declaration) => {
				debug!(
					"Found module declaration for {:?}",
					id_table.get_by_key(&declaration.id).unwrap()
				);
				declaration.analyze(id_table, nc_table, &mut modules_declared)?
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
				modules_implemented.insert(implementation.id, implementation);
			},
			PackageDeclaration(package) => {
				packaged_paths.push(package.analyze(id_table, &path_from_root, present_files)?)
			},
			UseStatement(_) => (),
		};
	}
	for (name, (m, loc)) in &modules_declared {
		if m.is_generic {
			match modules_implemented.get(name) {
				None => {
					return Err(miette::Report::new(
						crate::ProvidesCompilerDiagnostic::to_diagnostic_builder(
							&crate::analyzer::SemanticError::GenericModuleImplementationNotFound,
						)
						.label(
							*loc,
							format!(
								"Module {:?} is not implemented in the same file as it is declared",
								id_table.get_by_key(name).unwrap()
							)
							.as_str(),
						)
						.build(),
					))
				},
				Some(implementation) => {
					generic_modules.insert(name, *implementation);
					modules_implemented.remove(name);
				},
			}
		}
	}

	// next stage of analysis
	let mut ctx: AnalyzerContext<'_> = AnalyzerContext {
		id_table,
		nc_table,
		modules_declared: &modules_declared,
		generic_modules: &generic_modules,
		scope_map: HashMap::new(),
	};
	for implementation in modules_implemented.values() {
		implementation.analyze(&mut ctx)?;
	}
	debug!("Modules: {:?}", modules_declared.len());
	Ok(packaged_paths)
}
#[derive(Debug, Clone)]
pub struct AnalyzerContext<'a> {
	pub id_table: &'a IdTable,
	pub nc_table: &'a crate::lexer::NumericConstantTable,
	pub modules_declared: &'a HashMap<IdTableKey, (ModuleDeclared, SourceSpan)>,
	pub generic_modules: &'a HashMap<&'a IdTableKey, &'a ModuleImplementation>,
	pub scope_map: HashMap<SourceSpan, usize>,
}

impl ModuleImplementation {
	pub fn analyze(&self, ctx: &mut AnalyzerContext) -> miette::Result<()> {
		use crate::parser::ast::ModuleImplementationStatement::*;
		let mut scope = ModuleImplementationScope::new();
		let id = scope.new_scope(None);
		match &self.statement {
			ModuleImplementationBlockStatement(block) => block.analyze(ctx, &mut scope, id)?,
			_ => unreachable!(),
		};
		Ok(())
	}
}

impl ModuleImplementationBlockStatement {
	pub fn analyze(
		&self,
		ctx: &mut AnalyzerContext,
		scope: &mut ModuleImplementationScope,
		scope_id: usize,
	) -> miette::Result<()> {
		ctx.scope_map.insert(self.location, scope_id);
		use crate::parser::ast::ModuleImplementationStatement::*;
		for statement in &self.statements {
			match statement {
				VariableBlock(block) => block.analyze(CombinedQualifiers::new(), ctx.id_table, scope, scope_id)?,
				VariableDefinition(definition) => {
					definition.analyze(CombinedQualifiers::new(), ctx.id_table, scope, scope_id)?
				},
				AssignmentStatement(_) => todo!(),
				IfElseStatement(_) => todo!(),
				IterationStatement(_) => todo!(),
				InstantiationStatement(_) => todo!(),
				ModuleImplementationBlockStatement(block) => {
					let id = scope.new_scope(Some(scope_id));
					block.analyze(ctx, scope, id)?;
				},
			}
		}
		Ok(())
	}
}
impl VariableDefinition {
	pub fn analyze(
		&self,
		mut already_combined: CombinedQualifiers,
		id_table: &IdTable,
		scope: &mut ModuleImplementationScope,
		scope_id: usize,
	) -> miette::Result<()> {
		already_combined = analyze_qualifiers(&self.type_declarator.qualifiers, already_combined)?;
		analyze_specifier_implementation(&self.type_declarator.specifier, &already_combined)?;
		for direct_initializer in &self.initializer_list {
			if let Some(variable) = scope.get_variable_in_scope(scope_id, &direct_initializer.declarator.name) {
				return Err(miette::Report::new(
					SemanticError::DuplicateVariableDeclaration
						.to_diagnostic_builder()
						.label(
							direct_initializer.declarator.get_location(),
							format!(
								"Variable with name \"{}\" declared here, was already declared before.",
								id_table.get_by_key(&direct_initializer.declarator.name).unwrap()
							)
							.as_str(),
						)
						.label(
							variable.location,
							format!(
								"Here variable \"{}\" was declared before.",
								id_table.get_by_key(&direct_initializer.declarator.name).unwrap()
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
		mut already_combined: CombinedQualifiers,
		id_table: &IdTable,
		scope: &mut ModuleImplementationScope,
		scope_id: usize,
	) -> miette::Result<()> {
		already_combined = analyze_qualifiers(&self.types, already_combined)?;
		for statement in &self.statements {
			statement.analyze(already_combined.clone(), id_table, scope, scope_id)?;
		}
		Ok(())
	}
}
impl VariableBlockStatement {
	pub fn analyze(
		&self,
		already_combined: CombinedQualifiers,
		id_table: &IdTable,
		scope: &mut ModuleImplementationScope,
		scope_id: usize,
	) -> miette::Result<()> {
		match self {
			VariableBlockStatement::VariableBlock(block) => {
				block.analyze(already_combined, id_table, scope, scope_id)?
			},
			VariableBlockStatement::VariableDefinition(definition) => {
				definition.analyze(already_combined, id_table, scope, scope_id)?
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
		match q {
			TypeQualifier::Signed { location } => {
				if let Some(prev) = &already_combined.signed {
					report_duplicated_qualifier(location, prev, "signed")?;
				}
				already_combined.signed = Some(*location);
			},
			TypeQualifier::Unsigned { location } => {
				if let Some(prev) = &already_combined.unsigned {
					report_duplicated_qualifier(location, prev, "unsigned")?;
				}
				already_combined.unsigned = Some(*location);
			},
			TypeQualifier::Const { location } => {
				if let Some(prev) = &already_combined.constant {
					report_duplicated_qualifier(location, prev, "const")?;
				}
				already_combined.constant = Some(*location);
			},
			TypeQualifier::Clock { location } => {
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
			TypeQualifier::Input { location } => {
				if let Some(prev) = &already_combined.input {
					report_duplicated_qualifier(location, prev, "input")?;
				}
				already_combined.input = Some(*location);
			},
			TypeQualifier::Output { location } => {
				if let Some(prev) = &already_combined.output {
					report_duplicated_qualifier(location, prev, "output")?;
				}
				already_combined.output = Some(*location);
			},
			TypeQualifier::Async { location } => {
				if let Some(prev) = &already_combined.asynchronous {
					report_duplicated_qualifier(location, prev, "async")?;
				}
				already_combined.asynchronous = Some(*location);
			},
			TypeQualifier::Tristate { location: _ } => (),
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
	location1: &SourceSpan,
	location2: &SourceSpan,
	qualifier_name: &str,
	specifier_name: &str,
) -> miette::Result<()> {
	Err(miette::Report::new(
		SemanticError::ContradictingSpecifier
			.to_diagnostic_builder()
			.label(
				*location1,
				format!(
					"This \"{}\"qualifier contradicts the \"{}\" specifier of this variable",
					qualifier_name, specifier_name
				)
				.as_str(),
			)
			.label(
				*location2,
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
