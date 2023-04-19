struct DiagnosticBuffer {
	buffer: Vec<Box<dyn Diagnostic>>,
}
impl DiagnosticBuffer {
	fn push_diagnostic(&mut self, diag: Box<dyn Diagnostics>) {
		self.buffer.push(diag);
	}
	fn new() -> Box<DiagnosticBuffer> {
		Box::new(DiagnosticBuffer { buffer: vec![] })
	}
}
