mod pretty_printable;

use crate::analyzer::{ModuleDeclarationScope, SemanticError, VariableDeclared, analyze_qualifiers, analyze_specifier};
use crate::lexer::IdTable;
use crate::{lexer::CommentTableKey, analyzer::CombinedQualifiers};
use crate::parser::ast::SourceLocation;
use crate::{SourceSpan, ProvidesCompilerDiagnostic};


use super::{DirectDeclarator, TypeDeclarator, TypeQualifier, TypeSpecifier};
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct VariableDeclarationStatement {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub direct_declarators: Vec<DirectDeclarator>,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct ModuleDeclarationVariableBlock {
	pub metadata: Vec<CommentTableKey>,
	pub types: Vec<TypeQualifier>,
	pub statements: Vec<ModuleDeclarationStatement>,
	pub location: SourceSpan,
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
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
					{
						match declaration.type_declarator.specifier{
								TypeSpecifier::Int { .. } | TypeSpecifier::Bool { .. } =>(),
								_ =>return Err(
									miette::Report::new(
										SemanticError::MissingDirectionQualifier.to_diagnostic_builder()
												.label(direct_declarator.get_location(), format!("Variable with name \"{}\" is not qualified as either output or input", id_table.get_by_key(&direct_declarator.name).unwrap()).as_str())	
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






impl SourceLocation for ModuleDeclarationStatement {
	fn get_location(&self) -> SourceSpan {
		use self::ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement (declaration) => declaration.location,
			VariableBlock (block) => block.location,
		}
	}
}

