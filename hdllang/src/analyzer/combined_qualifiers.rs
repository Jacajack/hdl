use crate::{parser::ast::Expression, ProvidesCompilerDiagnostic, SourceSpan};

use super::SemanticError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CombinedQualifiers {
	pub signed: Option<SourceSpan>,
	pub unsigned: Option<SourceSpan>,
	pub constant: Option<SourceSpan>,
	pub comb: Option<(Vec<Expression>, SourceSpan)>,
	pub input: Option<SourceSpan>,
	pub output: Option<SourceSpan>,
	pub clock: Option<SourceSpan>,
	pub asynchronous: Option<SourceSpan>,
	pub synchronous: Option<(Vec<Expression>, SourceSpan)>,
}

impl CombinedQualifiers {
	pub fn new() -> Self {
		Self {
			signed: None,
			unsigned: None,
			constant: None,
			comb: None,
			input: None,
			output: None,
			clock: None,
			asynchronous: None,
			synchronous: None,
		}
	}
	pub fn check_for_contradicting(&self) -> miette::Result<()> {
		match self {
			CombinedQualifiers {
				signed: Some(x),
				unsigned: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "signed", "unsigned")?;
			},
			CombinedQualifiers {
				input: Some(x),
				output: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "input", "output")?;
			},
			CombinedQualifiers {
				constant: Some(x),
				comb: Some((_, y)),
				..
			} => {
				report_contradicting_qualifier(x, y, "const", "comb")?;
			},
			CombinedQualifiers {
				constant: Some(x),
				synchronous: Some((_, y)),
				..
			} => {
				report_contradicting_qualifier(x, y, "const", "sync")?;
			},
			CombinedQualifiers {
				constant: Some(x),
				asynchronous: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "const", "async")?;
			},
			CombinedQualifiers {
				constant: Some(x),
				clock: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "const", "clock")?;
			},
			CombinedQualifiers {
				comb: Some((_, x)),
				synchronous: Some((_, y)),
				..
			} => {
				report_contradicting_qualifier(x, y, "comb", "sync")?;
			},
			CombinedQualifiers {
				comb: Some((_, x)),
				asynchronous: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "comb", "async")?;
			},
			CombinedQualifiers {
				comb: Some((_, x)),
				clock: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "comb", "clock")?;
			},
			CombinedQualifiers {
				synchronous: Some((_, x)),
				clock: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "sync", "clock")?;
			},
			CombinedQualifiers {
				synchronous: Some((_, x)),
				asynchronous: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "sync", "async")?;
			},
			CombinedQualifiers {
				asynchronous: Some(x),
				clock: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "async", "clock")?;
			},
			_ => (),
		};
		Ok(())
	}
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
