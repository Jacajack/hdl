use crate::CompilerDiagnostic;
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

impl From<CompilerError> for CompilerDiagnostic {
	fn from(err: CompilerError) -> Self {
		match err {
			CompilerError::LexerError(lexer_error) =>
				lexer_error.into(),

			CompilerError::IoError(ref io_error) =>
				CompilerDiagnostic::from_error(&err)
				.help(&io_error.to_string()),
		}
	}
}
