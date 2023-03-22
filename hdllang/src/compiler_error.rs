use thiserror::Error;
use crate::lexer::LexerError;

/// General compiler error
/// 
/// Should ultimately include errors from all parts of the compiler
#[derive(Copy, Clone, Debug, Error)]
pub enum CompilerError {
	#[error(transparent)]
	LexerError(#[from] LexerError),
}