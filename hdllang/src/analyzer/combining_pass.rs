use super::analyzer_pass::preamble::*;
use super::SemanticError;
use std::collections::HashMap;

use crate::core::{CompilerDiagnosticBuilder, IdTable};
use crate::lexer::IdTableKey;
use crate::parser::ast::{Expression, TopDefinition::*};
use crate::parser::ast::{ModuleDeclarationStatement, Modules, SourceLocation, TypeQualifier};

pub fn combine(
	id_table: &IdTable,
	ast: &Root,
	code: String,
	path_from_root: String,
	present_files: &mut HashMap<String, String>,
) -> miette::Result<Vec<String>> {
	use crate::CompilerError;
	use log::{debug, info};
	use sha256::try_digest;
	use std::path::Path;

	info!("Running combining pass");
	info!("Path from root: {}", path_from_root);

	let mut packaged_paths: Vec<String> = Vec::new();

	for def in &ast.definitions {
		match def {
			UseStatement { .. } => continue,
			ModuleDeclaration { id, statements, .. } => {
				debug!("Found module def for {:?}", id_table.get_by_key(id).unwrap());
				analyze_declaration(statements, &code)?;
				continue;
			},
			ModuleImplementation { id, .. } => {
				debug!("Found module impl for {:?}", id_table.get_by_key(id).unwrap());
				continue;
			},
			PackageDeclaration { path, .. } => match &path.modules {
				Modules::All => {
					return Err(miette::Report::new(
						CompilerDiagnosticBuilder::from_error(&SemanticError::MultiplePackageDeclaration)
							.label(def.get_location(), "You should declare only one package at a time.")
							.build(),
					)
					.with_source_code(code))
				},
				Modules::Specific { modules } => {
					if modules.len() != 1 {
						return Err(miette::Report::new(
							CompilerDiagnosticBuilder::from_error(&SemanticError::MultiplePackageDeclaration)
								.label(def.get_location(), "You should declare only one package at a time.")
								.build(),
						)
						.with_source_code(code));
					}

					let path = path.into_paths(id_table, &path_from_root).get(0).unwrap().clone();

					if !Path::new(&path).exists() {
						return Err(miette::Report::new(
							CompilerDiagnosticBuilder::from_error(&CompilerError::FileNotFound(path.clone()))
								.label(def.get_location(), &format!("Package not found: {}", path))
								.build(),
						)
						.with_source_code(code));
					}

					let hash = try_digest(&path).unwrap();
					debug!("File path: {}", path);
					debug!("Hash: {}", hash);
					debug!("Present files: {:?}", present_files);

					if let Some(file_name) = present_files.get(&hash) {
						debug!("File already packaged: {}", file_name);
						return Err(miette::Report::new(
							CompilerDiagnosticBuilder::from_error(&SemanticError::FilePackagedMultipleTimes)
								.label(def.get_location(), create_label_message(file_name).as_str())
								.build(),
						)
						.with_source_code(code));
					}

					debug!("File not packaged yet");
					present_files.insert(hash, path.clone());
					packaged_paths.push(path);
				},
			},
		}
	}
	Ok(packaged_paths)
}

fn create_label_message(file_name: &String) -> String {
	match file_name.as_str() {
		"root" => String::from("File already explicitly included via compiler command line argument"),
		_ => format!("File already packaged in {}", file_name),
	}
}

fn analyze_declaration(declaration_body: &Vec<ModuleDeclarationStatement>, code: &String) -> miette::Result<()> {
	let variables: HashMap<IdTableKey, Vec<TypeQualifier>> = HashMap::new();
	for statement in declaration_body {
		match statement {
			ModuleDeclarationStatement::VariableDeclarationStatement {
				type_declarator,
				direct_declarators,
				..
			} => analyze_qualifiers(&type_declarator.qualifiers, CombinedQualifiers::new(), code)?,
			ModuleDeclarationStatement::VariableBlock { types, statements, .. } => todo!(),
		};
	}
	Ok(())
}

struct CombinedQualifiers {
	signed: bool,
	unsigned: bool,
	tristate: bool,
	constant: bool,
	comb: Option<Expression>,
	input: bool,
	output: bool,
	asynchronous: bool,
}

impl CombinedQualifiers {
	fn new() -> Self {
		Self {
			signed: false,
			unsigned: false,
			tristate: false,
			constant: false,
			comb: None,
			input: false,
			output: false,
			asynchronous: false,
		}
	}
}
fn analyze_qualifiers(
	qualifiers: &Vec<TypeQualifier>,
	mut already_combined: CombinedQualifiers,
	code: &String,
) -> miette::Result<()> {
	for q in qualifiers {
		match q {
			TypeQualifier::Signed { location } => {
				match already_combined.signed {
					true => {
						return Err(miette::Report::new(
							CompilerDiagnosticBuilder::from_error(&SemanticError::DuplicateQualifier)
								.label(*location, "Duplicate qualifier of the same type")
								.build(),
						)
						.with_source_code(code.clone()))
					},
					false => already_combined.signed = true,
				};
				match already_combined.unsigned {
					true => {
						return Err(miette::Report::new(
							CompilerDiagnosticBuilder::from_error(&SemanticError::ContradictingQualifier)
								.label(*location, "Contradicting qualifier of the same type")
								.build(),
						)
						.with_source_code(code.clone()))
					},
					false => continue,
				}
			},
			TypeQualifier::Unsigned { location } => {
				match already_combined.unsigned {
					true => {
						return Err(miette::Report::new(
							CompilerDiagnosticBuilder::from_error(&SemanticError::DuplicateQualifier)
								.label(*location, "Duplicate qualifier of the same type")
								.build(),
						)
						.with_source_code(code.clone()))
					},
					false => already_combined.unsigned = true,
				};
				match already_combined.signed {
					true => {
						return Err(miette::Report::new(
							CompilerDiagnosticBuilder::from_error(&SemanticError::ContradictingQualifier)
								.label(*location, "This qualifier contradicts the \"signed\" qualifier found before")
								.help("Remove the contradicting qualifier")
								.build(),
						)
						.with_source_code(code.clone()))
					},
					false => continue,
				}
			},
			TypeQualifier::Tristate { location } => todo!(),
			TypeQualifier::Const { location } => todo!(),
			TypeQualifier::Clock { location } => todo!(),
			TypeQualifier::Comb { expression, location } => todo!(),
			TypeQualifier::Sync { expressions, location } => todo!(),
			TypeQualifier::Input { location } => todo!(),
			TypeQualifier::Output { location } => todo!(),
			TypeQualifier::Async { location } => todo!(),
		};
	}
	Ok(())
}
