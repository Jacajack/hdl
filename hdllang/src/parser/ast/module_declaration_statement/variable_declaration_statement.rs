use hirn::design::ModuleHandle;

use crate::analyzer::*;
use crate::core::NumericConstant;
use crate::lexer::CommentTableKey;
use crate::lexer::IdTable;
use crate::parser::ast::SourceLocation;
use crate::{ProvidesCompilerDiagnostic, SourceSpan};

use super::{DirectDeclarator, TypeDeclarator};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct VariableDeclarationStatement {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub direct_declarators: Vec<DirectDeclarator>,
	pub location: SourceSpan,
}
use crate::analyzer::Variable;
use crate::lexer::NumericConstantTable;
impl VariableDeclarationStatement {
	pub fn create_variable_declaration(
		&self,
		already_created: AlreadyCreated,
		nc_table: &NumericConstantTable,
		id_table: &IdTable,
		comment_table: &crate::lexer::CommentTable,
		context: &mut Box<LocalAnalyzerContext>,
		handle: &mut ModuleHandle,
	) -> miette::Result<()> {
		let mut kind =
			VariableKind::from_type_declarator(&self.type_declarator, 0, already_created, nc_table, id_table, context)?;
		match &mut kind {
			VariableKind::Signal(sig) => {
				if sig.is_auto() {
					return Err(miette::Report::new(
						SemanticError::AutoSpecifierInDeclaration
							.to_diagnostic_builder()
							.label(self.location, "Auto specifier is not allowed in variable declaration")
							.build(),
					));
				}
				if !sig.is_direction_specified() {
					return Err(miette::Report::new(
						SemanticError::MissingDirectionQualifier
							.to_diagnostic_builder()
							.label(self.location, "Signal must be either input or output")
							.build(),
					));
				}
				if !sig.is_sensititivity_specified() {
					return Err(miette::Report::new(
						SemanticError::MissingSensitivityQualifier
							.to_diagnostic_builder()
							.label(self.location, "Signal must be either const, clock, comb, sync or async")
							.build(),
					));
				}
				if !sig.is_signedness_specified() {
					return Err(miette::Report::new(
						SemanticError::MissingSignednessQualifier
							.to_diagnostic_builder()
							.label(self.location, "Bus signal must be either signed or unsigned")
							.build(),
					));
				}
			},
			VariableKind::Generic(gen) => {
				gen.direction = Direction::Input(self.location);
				gen.width = Some(BusWidth::Evaluated(NumericConstant::new_from_value(64.into())));
				context.scope.mark_as_generic();
			},
			VariableKind::ModuleInstance(_) => unreachable!(),
		}
		let mut variables = Vec::new();

		for direct_declarator in &self.direct_declarators {
			let mut spec_kind = kind.clone();
			let mut dimensions = Vec::new();
			for array_declarator in &direct_declarator.array_declarators {
				let size = array_declarator.evaluate(nc_table, 0, &context.scope)?;
				let id = context.scope.add_expression(0, array_declarator.clone());
				match &size {
					Some(val) => {
						if val.value <= num_bigint::BigInt::from(0) {
							return Err(miette::Report::new(
								SemanticError::NegativeBusWidth
									.to_diagnostic_builder()
									.label(array_declarator.get_location(), "Array size must be positive")
									.build(),
							));
						}
						dimensions.push(BusWidth::EvaluatedLocated(val.clone(), id));
					},
					None => dimensions.push(BusWidth::Evaluable(id)),
				}
			}
			spec_kind = match spec_kind {
				VariableKind::Signal(mut sig) => {
					sig.dimensions = dimensions;
					VariableKind::Signal(sig)
				},
				VariableKind::Generic(gen) => {
					if dimensions.len() > 0 {
						panic!("Generic variable cannot have dimensions");
					}
					VariableKind::Generic(gen)
				},
				VariableKind::ModuleInstance(_) => unreachable!(),
			};
			variables.push(Variable {
				name: direct_declarator.name,
				metadata_comment: self.metadata.clone(),
				location: direct_declarator.get_location(),
				kind: spec_kind,
			});
		}
		for var in &variables {
			context.scope.declare_variable(var.clone(), nc_table, id_table, comment_table, handle)?;
		}
		Ok(())
	}
}
