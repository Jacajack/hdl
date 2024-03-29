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
	SemanticError(SemanticError),
	#[error(transparent)]
	HirnApiError(#[from] hirn::design::DesignError),
	#[error(transparent)]
	ElaborationError(#[from] hirn::elab::ElabError),
	#[error(transparent)]
	IoError(#[from] std::io::Error),
	#[error("File not found")]
	FileNotFound(String),
	#[error(transparent)]
	JsonError(#[from] serde_json::Error),
	#[error("Feature not implemented yet")]
	FeatureNotImplementedYet,
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
			JsonError(serde_error) => CompilerDiagnosticBuilder::from_error(&self)
				.help(&serde_error.to_string())
				.build(),
			FileNotFound(file_name) => CompilerDiagnosticBuilder::from_error(&self)
				.help(&format!("Make sure this file exists: {}", file_name))
				.build(),
			HirnApiError(err) => CompilerDiagnosticBuilder::from_error(&self)
				.help(&err.to_string())
				.build(),
			FeatureNotImplementedYet => CompilerDiagnosticBuilder::from_error(&self)
				.help("This feature is not implemented yet")
				.build(),
			ElaborationError(err) => CompilerDiagnosticBuilder::from_error(&self)
				.help(&err.to_string())
				.build(),
		}
	}
}
