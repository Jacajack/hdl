mod logos_lexer;
mod number_parser;
mod id_table;
mod comment_table;

use std::fmt;
use thiserror::Error;
use crate::SourceSpan;
use crate::CompilerDiagnostic;
use crate::ProvidesCompilerDiagnostic;
pub use id_table::{IdTable, IdTableKey};
pub use comment_table::{CommentTable, CommentTableKey};
pub use logos_lexer::LogosLexer;
pub use number_parser::NumberParseError;

/// Lexer token type
/// In this case, it's defined by the Logos-based lexer implementation.
/// Not super pretty, but it's the simplest solution currently.
pub type TokenKind = logos_lexer::TokenKind;

/// Types of lexer errors
#[derive(Copy, Clone, Error, Debug)]
pub enum LexerErrorKind {
	/// Lexer couldn't match token to any regex
	#[error("Invalid token")]
	InvalidToken,

	/// Numeric constant could not be parsed correctly
	#[error("Invalid number token")]
	InvalidNumber(NumberParseError),

	/// Unterminated block comment 
	#[error("Unterminated block comment")]
	UnterminatedBlockComment,
}

/// Lexer error
#[derive(Copy, Clone, Error, Debug)]
pub struct LexerError {
	pub range: SourceSpan,
	pub kind: LexerErrorKind,
}

impl fmt::Display for LexerError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.kind)
	}
}

impl ProvidesCompilerDiagnostic for LexerError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		match self.kind {
			LexerErrorKind::InvalidNumber(parse_err) => 
				parse_err
					.to_diagnostic()
					.shift_labels(self.range.offset()),

			LexerErrorKind::UnterminatedBlockComment =>
				CompilerDiagnostic::from_error(&self)
				.label(self.range, "This comment never ends")
				.help("Did you forget to use '*/"),

			LexerErrorKind::InvalidToken =>
				CompilerDiagnostic::from_error(&self)
				.label(self.range, "This token doesn't make sense")
				.help("This is neither a keyword, an identifier nor a valid numeric constant"),
		}
	}
}

/// All language keywords
#[derive(Debug, Clone, Copy)]
pub enum KeywordKind {
	Auto,
	Bus,
	Bool,
	Clock,
	ClockGate,
	Comb,
	Conditional,
	Const,
	Default,
	Else,
	Enum,
	FfSync,
	For,
	If,
	In,
	Impl,
	Input,
	Int,
	Match,
	Module,
	Node,
	Output,
	Register,
	Signed,
	Sync,
	Async,
	Tristate,
	TristateBuffer,
	Unsigned,
	Unused,
	Wire,
}

/// All language punctuators
#[derive(Debug, Clone, Copy)]
pub enum PunctuatorKind {
	Assignment,      // =
	AssignmentAnd,   // &=
	AssignmentPlus,  // +=
	AssignmentXor,   // ^=
	AssignmentOr,    // |=
	Asterisk,        // *
	BitwiseAnd,      // &
	BitwiseNot,      // ~
	BitwiseOr,       // |
	BitwiseXor,      // ^
	Colon,           // :
	Comma,           // ,
	Dot,             // .
	Equals,          // ==
	Greater,         // >
	GreaterEqual,    // >=
	Implies,         // =>
	LBrace,          // {
	LBracket,        // [
	Less,            // <
	LessEqual,       // <=
	LogicalAnd,      // &&
	LogicalNot,      // !
	LogicalOr,      // ||
	LPar,            // (
	LShift,          // <<
	Minus,           // -
	Modulo,          // %
	NotEquals,       // !=
	Plus,            // +
	QuestionMark,    // ?
	RBrace,          // }
	RBracket,        // ]
	RPar,            // )
	RShift,          // >>	
	Semicolon,       // ;
	Slash,           // /
	PlusColon,		 // +:
}

/// Token as produced by the lexer (token kind + source location)
#[derive(Debug, Copy, Clone)]
pub struct Token {
	/// Type of the token
	pub kind: TokenKind,    

	/// Source code location
	pub range: SourceSpan, 
}

/// Abstract lexer
pub trait Lexer<'source> {
	/// Creates a lexer for provided source code
	fn new(source: &'source str) -> Self;

	/// Processes the text and returns a vector of tokens
	fn process(&mut self) -> Result<Vec<Token>, LexerError>;

	/// Access the ID table
	fn id_table(&self) -> &IdTable;
}
