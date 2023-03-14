use std::fmt;
use std::fmt::Display;
use miette::{Diagnostic, Severity, LabeledSpan};

#[derive(Clone, Debug)]
pub struct CompilerDiagnostic<ErrorType = ()> {
	severity: Severity,
	error_text: String,
	help_text: Option<String>,
	error_code: Option<String>,
	labels: Vec<LabeledSpan>,
	underlying: Option<ErrorType>,
}

impl fmt::Display for CompilerDiagnostic {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.error_text)
	}
}

impl std::error::Error for CompilerDiagnostic {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {None}
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

impl<ErrorType: std::error::Error + Clone> CompilerDiagnostic<ErrorType> {
	pub fn new(severity: miette::Severity) -> Self {
		Self {
			severity,
			help_text: None,
			error_text: "none".into(),
			error_code: None,
			labels: Vec::new(),
			underlying: None,
		}
	}

	pub fn from_error(err: &ErrorType) -> Self {
		let mut diag = Self::new_error();
		diag.error_text = err.to_string();
		diag.underlying = Some(err.clone());
		diag
	}

	pub fn new_error() -> Self {
		Self::new(miette::Severity::Error)
	}

	pub fn new_warning() -> Self {
		Self::new(miette::Severity::Warning)
	}

	pub fn new_info() -> Self {
		Self::new(miette::Severity::Advice)
	}

	pub fn label(&mut self, label: miette::LabeledSpan) -> &Self {
		self.labels.push(label);
		self
	}

	pub fn help(&mut self, help: &str) -> &Self {
		self.help_text = Some(help.into());
		self
	}

	pub fn get_underlying(&self) -> &Option<ErrorType> {
		&self.underlying
	}
}
