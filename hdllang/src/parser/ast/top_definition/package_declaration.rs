use crate::ProvidesCompilerDiagnostic;
use crate::core::{CommentTableKey, SourceSpan, IdTable};
use crate::parser::ast::ImportPath;
use crate::analyzer::SemanticError;

use std::collections::HashMap;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct PackageDeclaration {
	pub metadata: Vec<CommentTableKey>,
	pub path: ImportPath,
	pub location: SourceSpan,
}

impl PackageDeclaration {
	pub fn analyze(
		&self,
		id_table: &IdTable,
		path_from_root: &String,
		present_files: &mut HashMap<String, String>,
	) -> miette::Result<String> {
		use crate::parser::ast::Modules::*;
		use crate::CompilerError;
		use log::debug;
		use sha256::try_digest;
		use std::path::Path;
		match &self.path.modules {
			All => {
				return Err(miette::Report::new(
					SemanticError::MultiplePackageDeclaration
						.to_diagnostic_builder()
						.label(self.location, "You should declare only one package at a time.")
						.build(),
				))
			},
			Specific { modules } => {
				if modules.len() != 1 {
					return Err(miette::Report::new(
						SemanticError::MultiplePackageDeclaration
							.to_diagnostic_builder()
							.label(self.location, "You should declare only one package at a time.")
							.build(),
					));
				}

				let path = self.path.into_paths(id_table, &path_from_root).get(0).unwrap().clone();

				if !Path::new(&path).exists() {
					return Err(miette::Report::new(
						CompilerError::FileNotFound(path.clone())
							.to_diagnostic_builder()
							.label(self.location, &format!("Package not found: {}", path))
							.build(),
					));
				}

				let hash = try_digest(&path).unwrap();
				debug!("File path: {}", path);
				debug!("Hash: {}", hash);
				debug!("Present files: {:?}", present_files);

				if let Some(file_name) = present_files.get(&hash) {
					debug!("File already packaged: {}", file_name);
					return Err(miette::Report::new(
						SemanticError::FilePackagedMultipleTimes
							.to_diagnostic_builder()
							.label(self.location, create_label_message(file_name).as_str())
							.build(),
					));
				}

				debug!("File not packaged yet");
				present_files.insert(hash, path.clone());
				Ok(path.clone())
			},
		}
	}
}


fn create_label_message(file_name: &String) -> String {
	match file_name.as_str() {
		"root" => String::from("File already explicitly included via compiler command line argument"),
		_ => format!("File already packaged in {}", file_name),
	}
}
