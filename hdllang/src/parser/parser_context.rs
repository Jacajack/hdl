use crate::core::DiagnosticBuffer;
use miette::Diagnostic;
pub struct ParserContext{
    diagnostic_buffer: DiagnosticBuffer,
}
impl ParserContext{
    pub fn new() -> ParserContext{
        ParserContext{
            diagnostic_buffer: DiagnosticBuffer::new(),
        }
    }
    fn push_diagnostic(&mut self, diag: Box<dyn Diagnostic>) {
        self.diagnostic_buffer.push_diagnostic(diag);
    }
}