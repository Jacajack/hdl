use crate::SourceSpan;
use miette::{Diagnostic, LabeledSpan, Severity};
use std::error::Error;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

/// A generic compiler diagnostic message
#[derive(Clone, Debug)]
pub struct CompilerDiagnostic {
	severity: Severity,
	error_text: String,
	help_text: Option<String>,
	error_code: Option<String>,
	labels: Vec<LabeledSpan>,
}

/// Used to conveniently craft compiler error messages
/// Note: Builder is single use only. After build() is called it becomes invalid.
pub struct CompilerDiagnosticBuilder {
	diag: Option<CompilerDiagnostic>,
}

impl From<CompilerDiagnostic> for CompilerDiagnosticBuilder {
	fn from(diag: CompilerDiagnostic) -> Self {
		Self { diag: Some(diag) }
	}
}

impl CompilerDiagnosticBuilder {
	/// Creates an error diagnostic from an error type
	pub fn from_error<ErrorType>(err: &ErrorType) -> Self
	where
		ErrorType: Error,
	{
		Self::new_error(&err.to_string())
	}

	/// Creates a new error message
	pub fn new_error(msg: &str) -> Self {
		CompilerDiagnostic::new_error(msg).into()
	}

	/// Creates a new warning message
	pub fn new_warning(msg: &str) -> Self {
		CompilerDiagnostic::new_warning(msg).into()
	}

	/// Creates a new info message
	pub fn new_info(msg: &str) -> Self {
		CompilerDiagnostic::new_warning(msg).into()
	}

	/// Moves all attached labels by the specified offset
	pub fn shift_labels(mut self, offset: usize) -> Self {
		self.diag.as_mut().unwrap().shift_labels(offset);
		self
	}

	/// Adds a source code label
	pub fn label(mut self, span: SourceSpan, msg: &str) -> Self {
		self.diag.as_mut().unwrap().add_label(span, msg);
		self
	}

	/// Attaches an error code
	pub fn error_code(&mut self, code: &str) -> &mut Self {
		self.diag.as_mut().unwrap().set_error_code(code);
		self
	}

	/// Attaches a help message
	pub fn help(mut self, help: &str) -> Self {
		self.diag.as_mut().unwrap().set_help(help);
		self
	}

	/// Returns the new diagnostic
	/// Note: we could have a mutli-use builder but it would
	/// cost us a .clone() here.
	pub fn build(mut self) -> CompilerDiagnostic {
		self.diag.take().unwrap()
	}
}

impl Display for CompilerDiagnostic {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.error_text)
	}
}

impl Error for CompilerDiagnostic {
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
	fn new(severity: miette::Severity, msg: &str) -> Self {
		Self {
			severity,
			help_text: None,
			error_text: msg.into(),
			error_code: None,
			labels: Vec::new(),
		}
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
	pub fn add_label(&mut self, span: SourceSpan, msg: &str) {
		self.labels.push(miette::LabeledSpan::new_with_span(
			Some(String::from(msg)),
			<SourceSpan as Into<miette::SourceSpan>>::into(span),
		));
	}

	/// Moves all labels by the specified offset
	pub fn shift_labels(&mut self, offset: usize) {
		for label in &mut self.labels {
			let start = label.inner().offset().wrapping_add_signed(offset as isize);
			let new_span = start..start + label.inner().len();

			*label = miette::LabeledSpan::new_with_span(
				label.label().map(|s| s.to_string()),
				miette::SourceSpan::from(new_span),
			);
		}
	}

	/// Sets the help message
	pub fn set_help(&mut self, help: &str) {
		self.help_text = Some(help.into());
	}

	/// Set the error code
	pub fn set_error_code(&mut self, code: &str) {
		self.error_code = Some(code.into());
	}
}

/// Indicates that type can provide a CompilerDiagnostic message.
/// All compiler error types must implement this trait.
pub trait ProvidesCompilerDiagnostic: Into<CompilerDiagnostic> {
	/// Must be implemented by the error type
	fn to_diagnostic(&self) -> CompilerDiagnostic;

	/// Returns a diagnostic message builder - useful when you want to modify the message
	fn to_diagnostic_builder(&self) -> CompilerDiagnosticBuilder {
		self.to_diagnostic().into()
	}

	/// Returns a Miette report
	fn to_miette_report(&self) -> miette::Report {
		miette::Report::new(self.to_diagnostic())
	}
}

/// Implements ProvidesCompilerDiagnostic for reference types
/// for convenience
impl<T> ProvidesCompilerDiagnostic for &T
where
	T: ProvidesCompilerDiagnostic,
{
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		(*self).to_diagnostic()
	}
}

/// Implements conversions between CompilerDiagnostic and error types
impl<T> From<T> for CompilerDiagnostic
where
	T: ProvidesCompilerDiagnostic,
{
	fn from(err: T) -> Self {
		err.to_diagnostic()
	}
}
