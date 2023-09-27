mod pretty_printable;

use crate::analyzer::{analyze_qualifiers, analyze_specifier, ModuleDeclarationScope, SemanticError, VariableDeclared, VariableKind, GenericVariable, GenericVariableKind, Signal, SignalType, SignalSensitivity, SignalSignedness, report_contradicting_qualifier, Direction};
use crate::lexer::IdTable;
use crate::parser::ast::SourceLocation;
use crate::{analyzer::CombinedQualifiers, lexer::CommentTableKey};
use crate::{ProvidesCompilerDiagnostic, SourceSpan};

use super::{DirectDeclarator, TypeDeclarator, TypeQualifier, TypeSpecifier, type_declarator};
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct VariableDeclarationStatement {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub direct_declarators: Vec<DirectDeclarator>,
	pub location: SourceSpan,
}
use crate::analyzer::Variable;
use crate::lexer::NumericConstantTable;
impl VariableDeclarationStatement{
	pub fn create_variable_declaration(&self, nc_table: &NumericConstantTable) -> miette::Result<Vec<Variable>>{
		use TypeSpecifier::*;
		let kind: VariableKind = 	match &self.type_declarator.specifier{
    		Auto { location } => return Err(miette::Report::new(SemanticError::AutoSpecifierInDeclaration.to_diagnostic_builder().label(*location, "Auto specifier is not allowed in variable declaration").build())),
    		Int { .. } => {
				VariableKind::Generic(GenericVariable { value: None, kind: GenericVariableKind::Int })
			},
    		Wire { .. } => {
				todo!()
			},
    		Bool { .. } => {
				VariableKind::Generic(GenericVariable { value: None, kind: GenericVariableKind::Bool })
			},
    		Bus(bus) => {
				let mut sensitivity = SignalSensitivity::None;
				let mut direction = Direction::None;
				let mut signedness = SignalSignedness::None;
				use TypeQualifier::*;
				for qualifier in &self.type_declarator.qualifiers{
					match qualifier{
        				Signed { location } => {
							match signedness{
								SignalSignedness::None => (),
								SignalSignedness::Signed(_) => (),
								_ => return Err(miette::Report::new(SemanticError::ContradictingQualifier.to_diagnostic_builder().label(*location, "Contradicting signedness").build())),
							};
							signedness = SignalSignedness::Signed(*location);
						},
        				Unsigned { location } => {
							match signedness{
								SignalSignedness::None => (),
								SignalSignedness::Unsigned(_) => (),
								_ => return Err(miette::Report::new(SemanticError::ContradictingQualifier.to_diagnostic_builder().label(*location, "Contradicting signedness").build())),
							};
							signedness = SignalSignedness::Unsigned(*location);
						},
        				Tristate { .. } => (),
        				Const { location } => todo!(),
        				Clock { location } => todo!(),
        				Comb(_) => todo!(),
        				Sync(_) => todo!(),
        				Input { location } => {
							match direction{
								Direction::None => (),
								Direction::Input(_) => (),
								_ => return Err(miette::Report::new(SemanticError::ContradictingQualifier.to_diagnostic_builder().label(*location, "Contradicting direction").build())),
							};
							direction = Direction::Input(*location);
						},
        				Output { location } => {
							match direction{
								Direction::None => (),
								Direction::Output(_) => (),
								_ => return Err(miette::Report::new(SemanticError::ContradictingQualifier.to_diagnostic_builder().label(*location, "Contradicting direction").build())),
							};
							direction = Direction::Output(*location);
						},
        				Async { location } => {
							match sensitivity{
								SignalSensitivity::None => (),
								SignalSensitivity::Async(_) => (),
								_ => return Err(miette::Report::new(SemanticError::ContradictingQualifier.to_diagnostic_builder().label(*location, "Contradicting signal sensitivity").build())),
							};
							sensitivity = SignalSensitivity::Async(*location);
						},
    				}
				}
				VariableKind::Signal(Signal{
					signal_type: SignalType::Bus{
						width: bus.width.evaluate_in_declaration(nc_table)?.value,
						signedness: SignalSignedness::None
					},
					sensitivity,
					direction,
				})
			},
		
		};

		let mut variables = Vec::new();

		for direct_declarator in &self.direct_declarators{
			let mut dimensions = Vec::new();
			for array_declarator in &direct_declarator.array_declarators{
				dimensions.push(array_declarator.evaluate_in_declaration(nc_table)?.value);
			}
			variables.push(Variable{
				name: direct_declarator.name,
				dimensions,
				location: direct_declarator.get_location(),
				kind: kind.clone(),
			});
		}
		Ok(variables)
	}
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
	VariableDeclarationStatement(VariableDeclarationStatement),
	VariableBlock(ModuleDeclarationVariableBlock),
}

impl ModuleDeclarationStatement {
	pub fn analyze(
		&self,
		mut already_combined: CombinedQualifiers,
		scope: &mut ModuleDeclarationScope,
		id_table: &IdTable,
	) -> miette::Result<()> {
		use ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement(declaration) => {
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
						));
					}
					if already_combined.input.is_none() && already_combined.output.is_none() {
						match declaration.type_declarator.specifier {
							TypeSpecifier::Int { .. } | TypeSpecifier::Bool { .. } => (),
							_ => {
								return Err(miette::Report::new(
									SemanticError::MissingDirectionQualifier
										.to_diagnostic_builder()
										.label(
											direct_declarator.get_location(),
											format!(
												"Variable with name \"{}\" is not qualified as either output or input",
												id_table.get_by_key(&direct_declarator.name).unwrap()
											)
											.as_str(),
										)
										.build(),
								))
							},
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
			VariableBlock(block) => {
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
			VariableDeclarationStatement(declaration) => declaration.location,
			VariableBlock(block) => block.location,
		}
	}
}
