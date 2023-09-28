mod pretty_printable;

use num_bigint::BigInt;

use crate::analyzer::*;
use crate::lexer::IdTable;
use crate::parser::ast::SourceLocation;
use crate::{analyzer::CombinedQualifiers, lexer::CommentTableKey};
use crate::{ProvidesCompilerDiagnostic, SourceSpan};

use super::{DirectDeclarator, TypeDeclarator, TypeQualifier, TypeSpecifier};
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
	pub fn create_variable_declaration(&self, mut already_created: AlreadyCreated, nc_table: &NumericConstantTable) -> miette::Result<Vec<Variable>>{
		use TypeSpecifier::*;
		let kind: VariableKind = 	match &self.type_declarator.specifier{
    		Auto { location } => return Err(miette::Report::new(SemanticError::AutoSpecifierInDeclaration.to_diagnostic_builder().label(*location, "Auto specifier is not allowed in variable declaration").build())),
    		Int { location } => {
				VariableKind::Generic(GenericVariable { value: None, kind: GenericVariableKind::Int(*location) })
			},
    		Wire { location } => {
				let wire_location = location;
				match &already_created.signedness{
        			SignalSignedness::Signed(prev) => report_qualifier_contradicting_specifier(location, prev, "signed", "wire")?,
        			SignalSignedness::Unsigned(prev) => report_qualifier_contradicting_specifier(location, prev, "unsigned", "wire")?,
        			SignalSignedness::None => (),
    			};
				use TypeQualifier::*;
				for qualifier in &self.type_declarator.qualifiers{
					match qualifier{
        				Signed { location } => report_qualifier_contradicting_specifier(location, wire_location, "signed", "wire")?,
        				Unsigned { location } => report_qualifier_contradicting_specifier(location, wire_location, "unsigned", "wire")?,
        				Tristate { location } => already_created.add_direction(Direction::Tristate(*location))?,
        				Const { location } => already_created.add_sensitivity(SignalSensitivity::Const(*location))?,
        				Clock { location } => already_created.add_sensitivity(SignalSensitivity::Clock(*location))?,
        				Comb(_) => todo!(),
        				Sync(_) => todo!(),
        				Input { location } => already_created.add_direction(Direction::Input(*location))?,
        				Output { location } => already_created.add_direction(Direction::Output(*location))?,
        				Async { location } => already_created.add_sensitivity(SignalSensitivity::Async(*location))?,
    				}
				}
				if already_created.direction == Direction::None{
					return Err(miette::Report::new(SemanticError::MissingDirectionQualifier.to_diagnostic_builder().label(*location, "Wire must be either input or output").build()));
				}
				VariableKind::Signal(Signal { signal_type: SignalType::Wire(*location), sensitivity: already_created.sensitivity, direction: already_created.direction })
			},
    		Bool { location } => {
				VariableKind::Generic(GenericVariable { value: None, kind: GenericVariableKind::Bool(*location) })
			},
    		Bus(bus) => {
				use TypeQualifier::*;
				for qualifier in &self.type_declarator.qualifiers{
					match qualifier{
        				Signed { location } => already_created.add_signedness(SignalSignedness::Signed(*location))?,
        				Unsigned { location } => already_created.add_signedness(SignalSignedness::Unsigned(*location))?,
        				Tristate { location } => already_created.add_direction(Direction::Tristate(*location))?,
        				Const { location } => already_created.add_sensitivity(SignalSensitivity::Const(*location))?,
        				Clock { location } => already_created.add_sensitivity(SignalSensitivity::Clock(*location))?,
        				Comb(_) => todo!(),
        				Sync(_) => todo!(),
        				Input { location } => already_created.add_direction(Direction::Input(*location))?,
        				Output { location } => already_created.add_direction(Direction::Output(*location))?,
        				Async { location } => already_created.add_sensitivity(SignalSensitivity::Async(*location))?,
    				}
				}
				if already_created.direction == Direction::None{
					return Err(miette::Report::new(SemanticError::MissingDirectionQualifier.to_diagnostic_builder().label(bus.location, "Bus must be either input or output").build()));
				}
				let width = bus.width.evaluate_in_declaration(nc_table)?.value;
				if width <= BigInt::from(0) {
					return Err(miette::Report::new(SemanticError::NegativeBusWidth.to_diagnostic_builder().label(bus.location, "Bus width must be positive").build()));
				}
				VariableKind::Signal(Signal{
					signal_type: SignalType::Bus{
						width,
						signedness: already_created.signedness,
						location: bus.location,
					},
					sensitivity: already_created.sensitivity,
					direction: already_created.direction,
				})
			},
		};

		let mut variables = Vec::new();

		for direct_declarator in &self.direct_declarators{
			let mut dimensions = Vec::new();
			for array_declarator in &direct_declarator.array_declarators{
				let size = array_declarator.evaluate_in_declaration(nc_table)?.value;
				if size <= BigInt::from(0){
					return Err(miette::Report::new(SemanticError::NegativeBusWidth.to_diagnostic_builder().label(array_declarator.get_location(), "Array size must be positive").build()));
				}
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
impl ModuleDeclarationVariableBlock {
	pub fn create_variable_declaration(&self, mut already_created: AlreadyCreated, nc_table: &NumericConstantTable) -> miette::Result<Vec<Variable>>{
		use TypeQualifier::*;
		for qualifier in &self.types{
			match qualifier{
				Signed { location } => already_created.add_signedness(SignalSignedness::Signed(*location))?,
				Unsigned { location } => already_created.add_signedness(SignalSignedness::Unsigned(*location))?,
				Tristate { location } => already_created.add_direction(Direction::Tristate(*location))?,
				Const { location } => already_created.add_sensitivity(SignalSensitivity::Const(*location))?,
				Clock { location } => already_created.add_sensitivity(SignalSensitivity::Clock(*location))?,
				Comb(_) => todo!(),
				Sync(_) => todo!(),
				Input { location } => already_created.add_direction(Direction::Input(*location))?,
				Output { location } => already_created.add_direction(Direction::Output(*location))?,
				Async { location } => already_created.add_sensitivity(SignalSensitivity::Async(*location))?,
			}
		}
		let mut variables = Vec::new();
		for statement in &self.statements{
			variables.append(&mut statement.create_variable_declaration(already_created.clone(), nc_table)?);
		}
		Ok(variables)
	}
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum ModuleDeclarationStatement {
	VariableDeclarationStatement(VariableDeclarationStatement),
	VariableBlock(ModuleDeclarationVariableBlock),
}

impl ModuleDeclarationStatement {
	pub fn create_variable_declaration(&self, already_created: AlreadyCreated, nc_table: &NumericConstantTable) -> miette::Result<Vec<Variable>>{
		use ModuleDeclarationStatement::*;
		match self{
			VariableDeclarationStatement(declaration) => declaration.create_variable_declaration(already_created, nc_table),
			VariableBlock(block) => block.create_variable_declaration(already_created, nc_table),
		}
	}
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
