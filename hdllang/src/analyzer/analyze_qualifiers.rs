use crate::analyzer::*;
use crate::lexer::IdTable;
use crate::ProvidesCompilerDiagnostic;

use crate::parser::ast::TypeQualifier;

pub fn analyze_qualifiers(
	qualifiers: &Vec<TypeQualifier>,
	mut already_created: AlreadyCreated,
	scope: &ModuleImplementationScope,
	current_scope: usize,
	id_table: &IdTable,
) -> miette::Result<AlreadyCreated> {
	use TypeQualifier::*;
	for qualifier in qualifiers {
		match qualifier {
			Signed { location } => already_created.add_signedness(SignalSignedness::Signed(*location))?,
			Unsigned { location } => already_created.add_signedness(SignalSignedness::Unsigned(*location))?,
			Tristate { location } => already_created.add_direction(Direction::Tristate(*location))?,
			Const { location } => already_created.add_sensitivity(SignalSensitivity::Const(*location))?,
			Clock { location } => already_created.add_sensitivity(SignalSensitivity::Clock(*location, None))?,
			Comb(comb) => {
				let mut sensitivity_list = ClockSensitivityList { list: Vec::new() };
				for signal in &comb.expressions {
					let sensitivity = signal.create_edge_sensitivity(current_scope, scope, id_table, comb.location)?;
					sensitivity_list.list.push(sensitivity);
				}
				already_created.add_sensitivity(SignalSensitivity::Comb(sensitivity_list, comb.location))?;
			},
			Sync(sync) => {
				let mut sensitivity_list = ClockSensitivityList { list: Vec::new() };
				match sync.expressions.len() {
					1 => {
						let name2 = sync.expressions[0].create_edge_sensitivity(
							current_scope,
							scope,
							id_table,
							sync.location,
						)?;
						sensitivity_list.list.push(name2);
					},
					2 => {
						let name2 = sync.expressions[0].create_edge_sensitivity(
							current_scope,
							scope,
							id_table,
							sync.location,
						)?;
						let name3 = sync.expressions[1].create_edge_sensitivity(
							current_scope,
							scope,
							id_table,
							sync.location,
						)?;
						if name3.clock_signal != name2.clock_signal {
							return Err(miette::Report::new(SemanticError::ForbiddenExpressionInSyncOrComb
									.to_diagnostic_builder()
									.label(sync.location, "\"sync\" qualifier must have at most consists of one variable and its negation")
									.build(),
								));
						}
						if name3.on_rising == name2.on_rising {
							return Err(miette::Report::new(SemanticError::ForbiddenExpressionInSyncOrComb
								.to_diagnostic_builder()
								.label(sync.location, "\"sync\" qualifier must have at most consists of one variable and its negation")
								.build(),
							));
						}
						sensitivity_list.list.push(name2);
						sensitivity_list.list.push(name3);
					},
					_ => {
						return Err(miette::Report::new(
							SemanticError::ForbiddenExpressionInSyncOrComb
								.to_diagnostic_builder()
								.label(
									sync.location,
									"\"sync\" qualifier must have at most consists of one variable and its negation",
								)
								.build(),
						))
					},
				}
				already_created.add_sensitivity(SignalSensitivity::Sync(sensitivity_list, sync.location))?;
			},
			Input { location } => already_created.add_direction(Direction::Input(*location))?,
			Output { location } => already_created.add_direction(Direction::Output(*location))?,
			Async { location } => already_created.add_sensitivity(SignalSensitivity::Async(*location))?,
		}
	}
	Ok(already_created)
}
