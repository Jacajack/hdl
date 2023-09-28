use std::collections::HashSet;

use num_bigint::BigInt;

use crate::{parser::ast::Expression, ProvidesCompilerDiagnostic, SourceSpan, lexer::IdTableKey, analyzer::report_duplicated_qualifier};

use super::SemanticError;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignalSignedness {
	Signed(SourceSpan),
	Unsigned(SourceSpan),
	None,
}
impl SignalSignedness{
	pub fn name(&self) -> &'static str{
		use SignalSignedness::*;
		match self{
			Signed(_) => "signed",
			Unsigned(_) => "unsigned",
			None => "none",
		}
	}
	pub fn location(&self) -> Option<&SourceSpan>{
		match self{
			SignalSignedness::Signed(x) => Some(x),
			SignalSignedness::Unsigned(x) => Some(x),
			SignalSignedness::None => None,
		}
	}

}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Direction {
	Input(SourceSpan),
	Output(SourceSpan),
	Tristate(SourceSpan),
	None
}

#[derive(Debug, Clone)]
pub enum SignalType {
	Bus {
		width: BigInt,
		signedness: SignalSignedness,
		location: SourceSpan,
	},
	Wire(SourceSpan)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct EdgeSensitivity {
	pub clock_signal: IdTableKey,
	pub on_rising: bool,
}

/// Determines sensitivity of a signal to certain clocks
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClockSensitivityList(HashSet<EdgeSensitivity>);
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SignalSensitivity {
	Async(SourceSpan),
	Comb(ClockSensitivityList, SourceSpan),
	Sync(ClockSensitivityList, SourceSpan),
	Clock(SourceSpan),
	Const(SourceSpan),
	/// if at the end of the analysis this is still None, it is an error
	None
}
impl SignalSensitivity{
	pub fn name(&self) -> &'static str{
		use SignalSensitivity::*;
		match self{
			Async(_) => "async",
			Comb(_, _) => "comb",
			Sync(_, _) => "sync",
			Clock(_) => "clock",
			Const(_) => "const",
			None => "none",
		}
	}
	pub fn location(&self) -> Option<&SourceSpan>{
		match self{
			SignalSensitivity::Async(x) => Some(x),
			SignalSensitivity::Comb(_, x) => Some(x),
			SignalSensitivity::Sync(_, x) => Some(x),
			SignalSensitivity::Clock(x) => Some(x),
			SignalSensitivity::Const(x) => Some(x),
			SignalSensitivity::None => None,
		}
	}
}
#[derive(Clone, Debug)]
pub struct AlreadyCreated{
	pub signedness: SignalSignedness,
	pub direction: Direction,
	pub sensitivity: SignalSensitivity,
}

impl AlreadyCreated{
	pub fn new() -> Self{
		Self{
			signedness: SignalSignedness::None,
			direction: Direction::None,
			sensitivity: SignalSensitivity::None,
		}
	}
	pub fn add_direction(&mut self, direction: Direction)->miette::Result<()>{
		use Direction::*;
		match (&self.direction, direction){
    		(Input(prev), Input(incoming)) => report_duplicated_qualifier(&incoming, prev, "input")?,
    		(Input(prev), Output(incoming)) => report_contradicting_qualifier(prev, &incoming, "input", "output")?,
    		(Input(prev), Tristate(incoming)) => report_contradicting_qualifier(prev, &incoming, "input", "tristate")?,
    		(Output(prev), Input(incoming)) => report_contradicting_qualifier(prev, &incoming, "output", "input")?,
    		(Output(prev), Output(incoming)) => report_duplicated_qualifier(&incoming, prev, "input")?,
    		(Output(prev), Tristate(incoming)) => report_contradicting_qualifier(prev, &incoming, "output", "tristate")?,
    		(Tristate(prev), Input(incoming)) => report_contradicting_qualifier(prev, &incoming, "tristate", "input")?,
    		(Tristate(prev), Output(incoming)) => report_contradicting_qualifier(prev, &incoming, "tristate", "output")?,
    		(Tristate(prev), Tristate(incoming)) => report_duplicated_qualifier(&incoming, prev, "input")?,
    		(None, Input(incoming)) => self.direction = Input(incoming),
    		(None, Output(incoming)) => self.direction = Output(incoming),
    		(None, Tristate(incoming)) => self.direction = Tristate(incoming),
    		(_, _) => (),
		}
		Ok(())
	}
	pub fn add_sensitivity(&mut self, sensitivity: SignalSensitivity) -> miette::Result<()>{
		use SignalSensitivity::*;
		match (&self.sensitivity, &sensitivity){
    		(Async(prev), Async(incoming)) | (Comb(_, prev), Comb(_, incoming)) | (Sync(_, prev), Sync(_, incoming)) | (Clock(prev), Clock(incoming)) | (Const(prev), Const(incoming)) => report_duplicated_qualifier(incoming, prev, sensitivity.name())?,
    		(None, _)=> self.sensitivity = sensitivity,
    		(_, None) => (),
			(_,_) => report_contradicting_qualifier(self.sensitivity.location().unwrap(), sensitivity.location().unwrap(), self.sensitivity.name(), sensitivity.name())?,

		};
		Ok(())
	}
	pub fn add_signedness(&mut self, signedness: SignalSignedness) ->miette::Result<()>{
		use SignalSignedness::*;
		match (&self.signedness, &signedness){
			(Signed(prev), Signed(incoming)) | (Unsigned(prev), Unsigned(incoming)) => report_duplicated_qualifier(incoming, prev, signedness.name())?,
			(None, _) => self.signedness = signedness,
			(_, None) => (),
			(_, _) => report_contradicting_qualifier(self.signedness.location().unwrap(), signedness.location().unwrap(), self.signedness.name(), signedness.name())?,
		};
		Ok(())
	}
}
#[derive(Clone, Debug)]
pub struct Signal{
	pub signal_type: SignalType,
	pub sensitivity: SignalSensitivity,
	pub direction: Direction,
}
#[derive(Clone, Debug)]
pub struct Variable{
	pub name: IdTableKey,
	pub dimensions: Vec<BigInt>,
	/// location of the variable declaration
	pub location: SourceSpan,
	pub kind: VariableKind,

}
#[derive(Clone, Debug)]
pub enum GenericVariableKind{
	Auto(SourceSpan),
	Int(SourceSpan),
	Bool(SourceSpan),
}
#[derive(Clone, Debug)]
pub struct GenericVariable{
	pub value: Option<BigInt>,
	pub kind: GenericVariableKind
}
#[derive(Clone, Debug)]
pub enum VariableKind{
	Signal(Signal),
	Generic(GenericVariable),
}
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
pub fn report_contradicting_qualifier(
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
