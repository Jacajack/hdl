mod pretty_printable;

use crate::analyzer::{CombinedQualifiers, ModuleDeclarationScope, ModuleDeclared, SemanticError};
use crate::lexer::IdTable;
use crate::parser::ast::{ImportPath, ModuleDeclarationStatement, ModuleImplementationStatement, SourceLocation};
use crate::ProvidesCompilerDiagnostic;
use crate::{lexer::CommentTableKey, lexer::IdTableKey, SourceSpan};
use std::collections::HashMap;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum TopDefinition {
	ModuleDeclaration(ModuleDeclaration),
	ModuleImplementation(ModuleImplementation),
	PackageDeclaration(PackageDeclaration),
	UseStatement(UseStatement),
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct ModuleDeclaration {
	pub metadata: Vec<CommentTableKey>,
	pub id: IdTableKey,
	pub statements: Vec<ModuleDeclarationStatement>,
	pub location: SourceSpan,
}

impl ModuleDeclaration {
	pub fn analyze(
		&self,
		id_table: &IdTable,
		nc_table: &crate::lexer::NumericConstantTable,
		modules_declared: &mut HashMap<IdTableKey, ModuleDeclared>,
	) -> miette::Result<()> {
		use log::debug;

		debug!(
			"Found module declaration for {:?}",
			id_table.get_by_key(&self.id).unwrap()
		);
		if let Some(module) = modules_declared.get(&self.id) {
			return Err(miette::Report::new(
				SemanticError::MultipleModuleDeclaration
					.to_diagnostic_builder()
					.label(module.location, "Module with this name is already declared here.")
					.label(self.location, "Module with this name is already declared.")
					.build(),
			));
		}
		let mut scope = ModuleDeclarationScope::new();
		for statement in &self.statements {
			statement.analyze(CombinedQualifiers::new(), &mut scope, id_table)?;
		}

		let is_generic = scope.is_generic();
		if is_generic {
			debug!(
				"Found generic module declaration for {:?}",
				id_table.get_by_key(&self.id).unwrap()
			);
		}
		else {
			scope.analyze(id_table, nc_table)?;
			debug!(
				"Found module declaration for {:?}",
				id_table.get_by_key(&self.id).unwrap()
			);
		}
		let m = ModuleDeclared {
			name: self.id,
			scope,
			is_generic,
			location: self.location,
		};
		modules_declared.insert(self.id, m);
		Ok(())
	}
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct ModuleImplementation {
	pub metadata: Vec<CommentTableKey>,
	pub id: IdTableKey,
	pub statement: ModuleImplementationStatement,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
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
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct UseStatement {
	pub metadata: Vec<CommentTableKey>,
	pub path: ImportPath,
	pub location: SourceSpan,
}

impl SourceLocation for TopDefinition {
	fn get_location(&self) -> SourceSpan {
		use self::TopDefinition::*;
		match self {
			ModuleImplementation(implementation) => implementation.location,
			PackageDeclaration(package) => package.location,
			UseStatement(use_statement) => use_statement.location,
			ModuleDeclaration(declaration) => declaration.location,
		}
	}
}
