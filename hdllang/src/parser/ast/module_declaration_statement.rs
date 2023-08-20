mod pretty_printable;

use crate::analyzer::{ModuleDeclarationScope, SemanticError, VariableDeclared};
use crate::lexer::IdTable;
use crate::{lexer::CommentTableKey, analyzer::CombinedQualifiers};
use crate::parser::ast::SourceLocation;
use crate::{SourceSpan, ProvidesCompilerDiagnostic};
use serde::{Deserialize, Serialize};

use super::{DirectDeclarator, TypeDeclarator, TypeQualifier, TypeSpecifier};
#[derive(Serialize, Deserialize, Debug)]
pub struct VariableDeclarationStatement {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub direct_declarators: Vec<DirectDeclarator>,
	pub location: SourceSpan,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ModuleDeclarationVariableBlock {
	pub metadata: Vec<CommentTableKey>,
	pub types: Vec<TypeQualifier>,
	pub statements: Vec<ModuleDeclarationStatement>,
	pub location: SourceSpan,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum ModuleDeclarationStatement {
	VariableDeclarationStatement (VariableDeclarationStatement),
	VariableBlock (ModuleDeclarationVariableBlock),
}

impl ModuleDeclarationStatement {
	pub fn analyze(
		&self,
		mut already_combined: CombinedQualifiers,
		scope: &mut ModuleDeclarationScope,
		id_table: &IdTable,
	) -> miette::Result<()> {
		match self {
			ModuleDeclarationStatement::VariableDeclarationStatement (declaration) => {
				already_combined = analyze_qualifiers(&declaration.type_declarator.qualifiers, already_combined)?;
				analyze_specifier(&declaration.type_declarator.specifier, &already_combined)?;

				for direct_declarator in &declaration.direct_declarators {
					if let Some(location) = scope.is_declared(&direct_declarator.name) {
						return Err(miette::Report::new(
							SemanticError::DuplicateVariableDeclaration
								.to_diagnostic_builder()
								.label(
									direct_declarator.get_location(),
									format!(
										"Variable with name \"{}\" declared here, was already declared before.",
										id_table.get_by_key(&direct_declarator.name).unwrap()
									)
									.as_str(),
								)
								.label(
									location,
									format!(
										"Here variable \"{}\" was declared before.",
										id_table.get_by_key(&direct_declarator.name).unwrap()
									)
									.as_str(),
								)
								.build(),
						)
						);
					}
					if already_combined.input.is_none()
						&& already_combined.output.is_none()
						&& already_combined.tristate.is_none()
					{
						match declaration.type_declarator.specifier{
								TypeSpecifier::Int { .. } | TypeSpecifier::Bool { .. } =>(),
								_ =>return Err(
									miette::Report::new(
										SemanticError::MissingDirectionQualifier.to_diagnostic_builder()
												.label(direct_declarator.get_location(), format!("Variable with name \"{}\" is not qualified as either output, input or tristate.", id_table.get_by_key(&direct_declarator.name).unwrap()).as_str())	
												.build()
												)),
							};
					}
					let variable = VariableDeclared {
						name: direct_declarator.name,
						qualifiers: already_combined.clone(),
						specifier: declaration.type_declarator.specifier.clone(),
						array: direct_declarator.array_declarators.clone(),
						array_compiled: None,
					};
					scope.declare(direct_declarator.name, variable, direct_declarator.get_location());
				}
			},
			ModuleDeclarationStatement::VariableBlock (block) => {
				already_combined = analyze_qualifiers(&block.types, already_combined)?;
				for statement in &block.statements {
					statement.analyze(already_combined.clone(), scope, id_table)?;
				}
			},
		};
		Ok(())
	}
}

fn analyze_qualifiers(
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
			TypeQualifier::Tristate { location } => {
				if let Some(prev) = &already_combined.tristate {
					report_duplicated_qualifier(location, prev, "tristate")?;
				}
				already_combined.tristate = Some(*location);
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
			TypeQualifier::Comb (comb) => {
				if let Some(prev) = &already_combined.comb {
					report_duplicated_qualifier(&comb.location, &prev.1, "comb")?;
				}
				already_combined.comb = Some((comb.expressions.clone(), comb.location));
			},
			TypeQualifier::Sync (sync) => {
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
		};

	}
	already_combined.check_for_contradicting()?;
	Ok(already_combined)
}

fn report_duplicated_qualifier(
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
	)
	)
}

fn analyze_specifier(
	type_specifier: &TypeSpecifier,
	already_combined: &CombinedQualifiers,
) -> miette::Result<()> {
	match type_specifier {
		TypeSpecifier::Auto { location } => {
			return Err(miette::Report::new(
				SemanticError::AutoSpecifierInDeclaration
					.to_diagnostic_builder()
					.label(*location, "This specifier is not allowed in a declaration")
					.build(),
			)
			)
		},
		TypeSpecifier::Wire { location } => {
			if let Some(location2) = &already_combined.signed {
				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
			} else if let Some(location2) = &already_combined.unsigned {
				report_qualifier_contradicting_specifier(location2, location, "unsigned", "wire")?;
			}
			Ok(())
		},
		_ => Ok(()),
	}
}

fn report_qualifier_contradicting_specifier(
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
	)
	)
}

impl SourceLocation for ModuleDeclarationStatement {
	fn get_location(&self) -> SourceSpan {
		use self::ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement (declaration) => declaration.location,
			VariableBlock (block) => block.location,
		}
	}
}

