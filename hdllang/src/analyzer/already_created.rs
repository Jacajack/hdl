use crate::{ProvidesCompilerDiagnostic, SourceSpan};

use super::{Direction, SemanticError, SignalSensitivity, SignalSignedness};

#[derive(Clone, Debug)]
pub struct AlreadyCreated {
	pub signedness: SignalSignedness,
	pub direction: Direction,
	pub sensitivity: SignalSensitivity,
}

impl AlreadyCreated {
	pub fn new() -> Self {
		Self {
			signedness: SignalSignedness::NoSignedness,
			direction: Direction::None,
			sensitivity: SignalSensitivity::NoSensitivity,
		}
	}
	pub fn add_direction(&mut self, direction: Direction) -> miette::Result<()> {
		use Direction::*;
		match (&self.direction, &direction) {
			(Input(prev), Input(incoming))
			| (Output(prev), Output(incoming))
			| (Tristate(prev), Tristate(incoming)) => report_duplicated_qualifier(&incoming, prev, direction.name())?,
			(None, _) => self.direction = direction,
			(_, None) => (),
			(..) => report_contradicting_qualifier(
				self.direction.location().unwrap(),
				direction.location().unwrap(),
				self.direction.name(),
				direction.name(),
			)?,
		}
		Ok(())
	}
	pub fn add_sensitivity(&mut self, sensitivity: SignalSensitivity) -> miette::Result<()> {
		use SignalSensitivity::*;
		match (&self.sensitivity, &sensitivity) {
			(Async(prev), Async(incoming))
			| (Comb(_, prev), Comb(_, incoming))
			| (Sync(_, prev), Sync(_, incoming))
			| (Clock(prev, _), Clock(incoming, _))
			| (Const(prev), Const(incoming)) => report_duplicated_qualifier(incoming, prev, sensitivity.name())?,
			(NoSensitivity, _) => self.sensitivity = sensitivity,
			(_, NoSensitivity) => (),
			(..) => report_contradicting_qualifier(
				self.sensitivity.location().unwrap(),
				sensitivity.location().unwrap(),
				self.sensitivity.name(),
				sensitivity.name(),
			)?,
		};
		Ok(())
	}
	pub fn add_signedness(&mut self, signedness: SignalSignedness) -> miette::Result<()> {
		use SignalSignedness::*;
		match (&self.signedness, &signedness) {
			(Signed(prev), Signed(incoming)) | (Unsigned(prev), Unsigned(incoming)) => {
				report_duplicated_qualifier(incoming, prev, signedness.name())?
			},
			(NoSignedness, _) => self.signedness = signedness,
			(_, NoSignedness) => (),
			(..) => report_contradicting_qualifier(
				self.signedness.location().unwrap(),
				signedness.location().unwrap(),
				self.signedness.name(),
				signedness.name(),
			)?,
		};
		Ok(())
	}
}
fn report_duplicated_qualifier(location: &SourceSpan, first_occurence: &SourceSpan, name: &str) -> miette::Result<()> {
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
	))
}

fn report_contradicting_qualifier(
	location_first: &SourceSpan,
	location_second: &SourceSpan,
	name_first: &str,
	name_second: &str,
) -> miette::Result<()> {
	Err(miette::Report::new(
		SemanticError::ContradictingQualifier
			.to_diagnostic_builder()
			.label(
				*location_first,
				format!(
					"This qualifier: \"{}\" contradicts the \"{}\" qualifier.",
					name_first, name_second
				)
				.as_str(),
			)
			.label(
				*location_second,
				format!(
					"This qualifier \"{}\" is mutually exclusive with \"{}\"",
					name_second, name_first
				)
				.as_str(),
			)
			.build(),
	))
}
