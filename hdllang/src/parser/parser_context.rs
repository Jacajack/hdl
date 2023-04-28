use crate::core::DiagnosticBuffer;
//use miette::Diagnostic;
pub struct ParserContext<'source> {
	pub diagnostic_buffer: Box<DiagnosticBuffer<'source>>,
}
//impl<'a> ParserContext<'a> {
//	//pub fn push_diagnostic(self, diag: Box<dyn Diagnostic>) {
//	//	self.diagnostic_buffer.push_diagnostic(diag);
//	//}
//}
