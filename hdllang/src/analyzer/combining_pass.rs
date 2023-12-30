use super::SemanticError;
use crate::analyzer::GlobalAnalyzerContext;
use log::{debug, info};
use std::collections::HashMap;

use crate::{core::*, parser::ast::*, ProvidesCompilerDiagnostic, SourceSpan};

pub fn combine<'a>(
	mut ctx: GlobalAnalyzerContext<'a>,
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
	let mut packaged_paths: Vec<String> = Vec::new();
	let mut modules_implemented: HashMap<IdTableKey, &ModuleImplementation> = HashMap::new();
	let mut generic_modules: HashMap<IdTableKey, &ModuleImplementation> = HashMap::new();

	for def in &ast.definitions {
		use crate::parser::ast::TopDefinition::*;
		match def {
			ModuleDeclaration(declaration) => {
				declaration.analyze(&mut ctx)?
			},
			ModuleImplementation(implementation) => {
				debug!(
					"Found module impl for {:?}",
					ctx.id_table.get_by_key(&implementation.id).unwrap()
				);
				if let Some(prev) = modules_implemented.get(&implementation.id) {
					return Err(miette::Report::new(
						SemanticError::MultipleModuleImplementations
							.to_diagnostic_builder()
							.label(
								implementation.location,
								format!(
									"Module {:?} is implemented multiple times",
									ctx.id_table.get_by_key(&implementation.id).unwrap()
								)
								.as_str(),
							)
							.label(
								prev.location,
								format!(
									"Module {:?} is implemented here",
									ctx.id_table.get_by_key(&prev.id).unwrap()
								)
								.as_str(),
							)
							.build(),
					));
				}

				modules_implemented.insert(implementation.id, &implementation);
			},
			PackageDeclaration(package) => {
				packaged_paths.push(package.analyze(&ctx.id_table, &path_from_root, present_files)?)
			},
			UseStatement(_) => (),
		};
	}
	for (name, m) in ctx.modules_declared.clone() {
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
								ctx.id_table.get_by_key(&name).unwrap()
							)
							.as_str(),
						)
						.build(),
					))
				},
				Some(implementation) => {
					ctx.generic_modules.insert(name.clone(), *implementation);
					modules_implemented.remove(&name);
				},
			}
		}
	}
	debug!("Modules: {:?}", ctx.modules_declared.len());
	// prepare for next stage of analysis

	Ok((packaged_paths, ctx, modules_implemented))
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
