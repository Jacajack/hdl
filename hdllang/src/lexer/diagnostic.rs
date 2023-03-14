use crate::CompilerDiagnostic;
use super::LexerError;

impl Into<CompilerDiagnostic<LexerError>> for LexerError {
	fn into(self) -> CompilerDiagnostic<LexerError> {
		// TODO implement this nicely
		CompilerDiagnostic::from_error(&self)
	}
}
