use crate::core::DiagnosticBuffer;
use miette::Diagnostic;
pub struct ParserContext<'a> {
	pub diagnostic_buffer: &'a mut DiagnosticBuffer,
}
impl<'a> ParserContext<'a> {
	pub fn push_diagnostic(&mut self, diag: Box<dyn Diagnostic>) {
		self.diagnostic_buffer.push_diagnostic(diag);
	}
}
