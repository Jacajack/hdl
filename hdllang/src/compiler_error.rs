use crate::CompilerDiagnostic;
use crate::ProvidesCompilerDiagnostic;
use thiserror::Error;
use crate::lexer::LexerError;

/// General compiler error
/// 
/// Should ultimately include errors from all parts of the compiler
#[derive(Debug, Error)]
pub enum CompilerError {
	#[error(transparent)]
	LexerError(#[from] LexerError),

	#[error(transparent)]
	IoError(#[from] std::io::Error)
}

impl ProvidesCompilerDiagnostic for CompilerError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		match self {
			CompilerError::LexerError(lexer_error) =>
				lexer_error.into(),

			CompilerError::IoError(ref io_error) =>
				CompilerDiagnostic::from_error(&self)
				.help(&io_error.to_string()),
		}
	}
}
