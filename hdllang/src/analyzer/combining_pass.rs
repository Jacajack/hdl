
use super::analyzer_pass::preamble::*;
use std::collections::HashMap;

use crate::core::IdTable;




pub fn combine(
	id_table: &IdTable,
	nc_table: &crate::lexer::NumericConstantTable,
	ast: &Root,
	mut path_from_root: String,
	present_files: &mut HashMap<String, String>,
) -> miette::Result<Vec<String>> {
	use log::{info, debug};
	
	info!("Running combining pass");
	info!("Path from root: {}", path_from_root);

	if path_from_root.as_str() == "" {
		path_from_root = ".".to_string();
	}

	let mut packaged_paths =  Vec::new();
	let mut modules_declared =  HashMap::new();
	let mut modules_implemented =  HashMap::new();
	use crate::parser::ast::TopDefinition::*;
	for def in &ast.definitions {
		match def {
   			ModuleDeclaration(declaration) => declaration.analyze(id_table, nc_table,  &mut modules_declared)?,
   			ModuleImplementation(implementation) => {
				debug!(
					"Found module impl for {:?}",
					id_table.get_by_key(&implementation.id).unwrap()
				);
				modules_implemented.insert(implementation.id, implementation);
			},
    		PackageDeclaration(package) => packaged_paths.push(package.analyze(id_table, &path_from_root, present_files)?),
    		UseStatement(_) => (),
		};
	}
	for (name,(m, loc)) in &modules_declared{
		if m.is_generic{
			match modules_implemented.get(name){
				None => return Err(miette::Report::new(crate::ProvidesCompilerDiagnostic::to_diagnostic_builder(&crate::analyzer::SemanticError::GenericModuleImplementationNotFound)
					.label(*loc, format!("Module {:?} is not implemented in the same file as it is declared", id_table.get_by_key(name).unwrap()).as_str())
					.build())),
				Some(_) => (),
			}
		}
	}
	debug!("Modules: {:?}", modules_declared.len());
	Ok(packaged_paths)
}