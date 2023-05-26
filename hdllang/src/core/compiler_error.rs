use crate::analyzer::SemanticError;
use crate::compiler_diagnostic::*;
use crate::lexer::LexerError;
use crate::parser::ParserError;
use thiserror::Error;

/// General compiler error
///
/// Should ultimately include errors from all parts of the compiler
#[derive(Debug, Error)]
pub enum CompilerError {
	#[error(transparent)]
	LexerError(#[from] LexerError),

	#[error(transparent)]
	ParserError(#[from] ParserError),

	#[error(transparent)]
	IoError(#[from] std::io::Error),

	#[error(transparent)]
	SemanticError(SemanticError),
}

impl ProvidesCompilerDiagnostic for CompilerError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		use CompilerError::*;
		match self {
			LexerError(lexer_error) => lexer_error.into(),

			ParserError(parser_error) => parser_error.into(),
			
			SemanticError(semantic_error) => semantic_error.into(),

			IoError(ref io_error) => CompilerDiagnosticBuilder::from_error(&self)
				.help(&io_error.to_string())
				.build(),
		}
	}
}
