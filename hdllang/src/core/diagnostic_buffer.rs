use crate::compiler_diagnostic::CompilerDiagnostic;
use std::fmt::Display;
pub struct DiagnosticBuffer {
	buffer: Vec<CompilerDiagnostic>,
}
impl DiagnosticBuffer {
	pub fn push_diagnostic(&mut self, diag: CompilerDiagnostic) {
		self.buffer.push(diag);
	}
	pub fn new() -> DiagnosticBuffer {
		DiagnosticBuffer { buffer: vec![] }
	}
}
impl Display for DiagnosticBuffer {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for diag in &self.buffer {
			writeln!(f, "{:?}", miette::Report::new((*diag).clone()))?;
		}
		write!(f, "")
	}
}
