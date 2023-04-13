use std::fmt;
use std::fmt::Display;
use std::fmt::Debug;
use std::error::Error;
use miette::{Diagnostic, Severity, LabeledSpan};
use crate::SourceSpan;

/// A generic compiler diagnostic message
#[derive(Clone, Debug)]
pub struct CompilerDiagnostic {
	severity: Severity,
	error_text: String,
	help_text: Option<String>,
	error_code: Option<String>,
	labels: Vec<LabeledSpan>,
}

impl Display for CompilerDiagnostic {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.error_text)
	}
}

impl Error for CompilerDiagnostic  {
	fn source(&self) -> Option<&(dyn Error + 'static)> {
		None
	}
}

impl Diagnostic for CompilerDiagnostic {
	fn code<'a>(&'a self) -> Option<Box<(dyn std::fmt::Display + 'a)>> {
		match &self.error_code {
			Some(msg) => Some(Box::new(miette::Report::msg(msg.clone()))),
			None => None,
		}
	}

	fn severity(&self) -> Option<miette::Severity> {
		Some(self.severity)
	}

	fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
		match &self.help_text {
			Some(msg) => Some(Box::new(miette::Report::msg(msg.clone()))),
			None => None,
		}
	}

	fn url<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
		None
	}

	fn source_code(&self) -> Option<&dyn miette::SourceCode> {
		None
	}

	fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
		Some(Box::new(self.labels.clone().into_iter()))
	}

	fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
		None
	}

	fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
		None
	}
}

impl CompilerDiagnostic {
	/// Creates a new diagnostic message
	pub fn new(severity: miette::Severity, msg: &str) -> Self {
		Self {
			severity,
			help_text: None,
			error_text: msg.into(),
			error_code: None,
			labels: Vec::new(),
		}
	}

	/// Creates an error diagnostic from an error type
	pub fn from_error<ErrorType>(err: &ErrorType) -> Self
		where ErrorType: Error 
	{
		Self::new_error(&err.to_string())
	}

	/// Creates a new error diagnostic
	pub fn new_error(msg: &str) -> Self {
		Self::new(miette::Severity::Error, msg)
	}

	/// Creates a new warning diagnostic
	pub fn new_warning(msg: &str) -> Self {
		Self::new(miette::Severity::Warning, msg)
	}

	/// Creates a new info diagnostic
	pub fn new_info(msg: &str) -> Self {
		Self::new(miette::Severity::Advice, msg)
	}

	/// Attaches source code label
	pub fn label(mut self, span: SourceSpan, msg: &str) -> Self {
		self.labels.push(
			miette::LabeledSpan::new_with_span(
				Some(String::from(msg)),
				<SourceSpan as Into<miette::SourceSpan>>::into(span)
			)
		);
		self
	}

	/// Attaches a help message
	pub fn help(mut self, help: &str) -> Self {
		self.help_text = Some(help.into());
		self
	}

	/// Attaches an error code
	pub fn error_code(mut self, code: &str) -> Self {
		self.error_code = Some(code.into());
		self
	}
}

pub trait ProvidesCompilerDiagnostic: Into<CompilerDiagnostic> {
	fn to_diagnostic(self) -> CompilerDiagnostic;
	fn to_report(self) -> miette::Report;
}

impl<T> ProvidesCompilerDiagnostic for T
where
	CompilerDiagnostic: From<T> + Diagnostic
{
	fn to_diagnostic(self) -> CompilerDiagnostic {
		CompilerDiagnostic::from(self)
	}	

	fn to_report(self) -> miette::Report {
		miette::Report::new(self.to_diagnostic())
	}
}