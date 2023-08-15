
use super::analyzer_pass::preamble::*;
use std::collections::HashMap;

use crate::core::{IdTable, SourceWithName};




pub fn combine(
	id_table: &IdTable,
	ast: &Root,
	source_code: SourceWithName,
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

	use crate::parser::ast::TopDefinition::*;
	for def in &ast.definitions {
		match def {
   			ModuleDeclaration(declaration) => declaration.analyze(id_table, &source_code, &mut modules_declared)?,
   			ModuleImplementation(implementation) => {
				debug!(
					"Found module impl for {:?}",
					id_table.get_by_key(&implementation.id).unwrap()
				);
			},
    		PackageDeclaration(package) => packaged_paths.push(package.analyze(id_table,&source_code, &path_from_root, present_files)?),
    		UseStatement(_) => (),
		};
	}
	debug!("Modules: {:?}", modules_declared.len());
	Ok(packaged_paths)
}