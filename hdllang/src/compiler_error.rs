use miette::Diagnostic;
use thiserror::Error;
use crate::lexer::LexerError;
use crate::analyzer::SemanticError;

/// General compiler error
/// 
/// Should ultimately include errors from all parts of the compiler
#[derive(Debug, Error, Diagnostic)]
pub enum CompilerError {
	#[error(transparent)]
	LexerError(#[from] LexerError),

	#[error(transparent)]
	#[diagnostic(code(hdllang::io_error))]
	IoError(#[from] std::io::Error),

	#[error(transparent)]
	SemanticError(SemanticError),
}