use crate::compiler_diagnostic::CompilerDiagnostic;

#[derive(Clone)]
pub struct DiagnosticBuffer {
	pub buffer: Vec<CompilerDiagnostic>,
}

impl DiagnosticBuffer {
	pub fn push_diagnostic(&mut self, diag: CompilerDiagnostic) {
		self.buffer.push(diag);
	}
	pub fn new() -> Self {
		Self { buffer: vec![] }
	}
}

