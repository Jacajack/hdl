use crate::compiler_diagnostic::*;
use crate::lexer::LexerError;
use thiserror::Error;

/// General compiler error
///
/// Should ultimately include errors from all parts of the compiler
#[derive(Debug, Error)]
pub enum CompilerError {
	#[error(transparent)]
	LexerError(#[from] LexerError),

	#[error(transparent)]
	IoError(#[from] std::io::Error),
}

impl ProvidesCompilerDiagnostic for CompilerError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		use CompilerError::*;
		match self {
			LexerError(lexer_error) => lexer_error.into(),

			IoError(ref io_error) => CompilerDiagnosticBuilder::from_error(&self)
				.help(&io_error.to_string())
				.build(),
		}
	}
}
