use std::fmt;
use std::fmt::Display;
use std::fmt::Debug;
use std::error::Error;
use miette::{Diagnostic, Severity, LabeledSpan};
use crate::SourceSpan;

/// A generic compiler diagnostic message
/// 
/// TODO From<T> if payload can be converted
#[derive(Clone)]
pub struct CompilerDiagnostic<ErrorType = ()> {
	severity: Severity,
	error_text: String,
	help_text: Option<String>,
	error_code: Option<String>,
	labels: Vec<LabeledSpan>,
	underlying: Option<ErrorType>,
}

impl<ErrorType: Debug> Debug for CompilerDiagnostic<ErrorType>  {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.debug_struct("CompilerDiagnostic")
			.field("severity", &self.severity)
			.field("error_text", &self.error_text)
			.field("help_text", &self.help_text)
			.field("error_code", &self.error_code)
			.field("underlying", &self.underlying)
			.finish()
	}
}

impl<ErrorType: Debug> Display for CompilerDiagnostic<ErrorType>  {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.error_text)
	}
}

impl<ErrorType: Debug> Error for CompilerDiagnostic<ErrorType>  {
	fn source(&self) -> Option<&(dyn Error + 'static)> {
		None
	}
}

impl<ErrorType: Debug> Diagnostic for CompilerDiagnostic<ErrorType> {
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

impl<ErrorType: Error + Debug + Clone> CompilerDiagnostic<ErrorType> {
	/// Creates a new diagnostic message
	pub fn new(severity: miette::Severity, msg: &str) -> Self {
		Self {
			severity,
			help_text: None,
			error_text: msg.into(),
			error_code: None,
			labels: Vec::new(),
			underlying: None,
		}
	}

	/// Creates an error diagnostic from an error type
	pub fn from_error(err: &ErrorType) -> Self {
		let mut diag = Self::new_error(&err.to_string());
		diag.underlying = Some(err.clone());
		diag
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

	/// Gives access to the underlying error type
	pub fn get_error(&self) -> &Option<ErrorType> {
		&self.underlying
	}
}
