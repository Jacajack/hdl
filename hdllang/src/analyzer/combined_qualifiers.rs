use crate::{SourceSpan, parser::ast::Expression, core::SourceWithName, ProvidesCompilerDiagnostic};

use super::SemanticError;

#[derive(Clone, Debug)]
pub struct CombinedQualifiers {
	pub signed: Option<SourceSpan>,
	pub unsigned: Option<SourceSpan>,
	pub tristate: Option<SourceSpan>,
	pub constant: Option<SourceSpan>,
	pub comb: Option<(Expression, SourceSpan)>,
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
			tristate: None,
			constant: None,
			comb: None,
			input: None,
			output: None,
			clock: None,
			asynchronous: None,
			synchronous: None,
		}
	}
	pub fn check_for_contradicting(&self, code: &SourceWithName)->miette::Result<()>{
		match self {
			CombinedQualifiers {
				signed: Some(x),
				unsigned: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "signed", "unsigned", code)?;
			},
			CombinedQualifiers {
				input: Some(x),
				tristate: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "input", "tristate", code)?;
			},
			CombinedQualifiers {
				input: Some(x),
				output: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "input", "output", code)?;
			},
			CombinedQualifiers {
				output: Some(x),
				tristate: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "output", "tristate", code)?;
			},
			CombinedQualifiers {
				constant: Some(x),
				comb: Some((_, y)),
				..
			} => {
				report_contradicting_qualifier(x, y, "const", "comb", code)?;
			},
			CombinedQualifiers {
				constant: Some(x),
				synchronous: Some((_, y)),
				..
			} => {
				report_contradicting_qualifier(x, y, "const", "sync", code)?;
			},
			CombinedQualifiers {
				constant: Some(x),
				asynchronous: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "const", "async", code)?;
			},
			CombinedQualifiers {
				constant: Some(x),
				clock: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "const", "clock", code)?;
			},
			CombinedQualifiers {
				comb: Some((_, x)),
				synchronous: Some((_, y)),
				..
			} => {
				report_contradicting_qualifier(x, y, "comb", "sync", code)?;
			},
			CombinedQualifiers {
				comb: Some((_, x)),
				asynchronous: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "comb", "async", code)?;
			},
			CombinedQualifiers {
				comb: Some((_, x)),
				clock: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "comb", "clock", code)?;
			},
			CombinedQualifiers {
				synchronous: Some((_, x)),
				clock: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "sync", "clock", code)?;
			},
			CombinedQualifiers {
				synchronous: Some((_, x)),
				asynchronous: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "sync", "async", code)?;
			},
			CombinedQualifiers {
				asynchronous: Some(x),
				clock: Some(y),
				..
			} => {
				report_contradicting_qualifier(x, y, "async", "clock", code)?;
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
	code: &SourceWithName,
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
	)
	.with_source_code(code.clone().into_named_source()))
}