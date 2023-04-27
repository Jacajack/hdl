use crate::compiler_diagnostic::CompilerDiagnostic;
use std::fmt::Display;

pub struct DiagnosticBuffer<'source> {
	buffer: Vec<(CompilerDiagnostic, Option<&'source String>)>,
}

impl<'source> DiagnosticBuffer<'source> {
	pub fn push_diagnostic(&mut self, diag: CompilerDiagnostic, source: Option<&'source String>) {
		self.buffer.push((diag,source));
	}
	pub fn new() -> Self {
		Self { buffer: vec![] }
	}
}

impl<'source> Display for DiagnosticBuffer<'source> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for (diag, source) in &self.buffer {
			let report = if let Some(src) = *source {
				miette::Report::new(diag.clone()).with_source_code(src.clone())
			}
			else {
				miette::Report::new(diag.clone())
			};
			writeln!(f, "{:?}", report)?;
		}
		write!(f, "")
	}
}
