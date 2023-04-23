use miette::Diagnostic;
use std::fmt::Display;
pub struct DiagnosticBuffer {
	buffer: Vec<Box<dyn Diagnostic>>,
}
impl DiagnosticBuffer {
	pub fn _push_diagnostic(&mut self, diag: Box<dyn Diagnostic>) {
		self.buffer.push(diag);
	}
	pub fn new() -> DiagnosticBuffer {
		DiagnosticBuffer { buffer: vec![] }
	}
}
impl Display for DiagnosticBuffer {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for i in 0..self.buffer.len() {
			write!(f, "{:?}\n", self.buffer[i])?;
		}
		write!(f, "")
	}
}
