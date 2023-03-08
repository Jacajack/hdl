mod logos_lexer;
pub use logos_lexer::LogosLexer;
use std::ops::Range;

/// Lexer token type
/// In this case, it's defined by the Logos-based lexer implementation.
/// Not super pretty, but it's the simplest solution currently.
pub type TokenKind = logos_lexer::TokenKind;

/// All language keywords
#[derive(Debug, Clone, Copy)]
pub enum KeywordKind {
	If,
	For,
	Wire,
	Signed,
	Unsigned,
}

/// All language punctuators
#[derive(Debug, Clone, Copy)]
pub enum PunctuatorKind {
	Plus,
	Minus,
	Comma,
	Colon,
	Semicolon,
	Asterisk,
	Slash,
	LBrace,
	RBrace,
	LBracket,
	RBracket,
	LPar,
	RPar,
	Percent,
}

/// Source code range (copyable)
#[derive(Debug, Copy, Clone)]
pub struct SourceRange {
	pub start: usize,
	pub end: usize,
}

impl SourceRange {
	fn new(r: &Range<usize>) -> SourceRange {
		SourceRange {
			start: r.start,
			end: r.end,
		}
	}
}

/// Token as produced by the lexer (token kind + source location)
#[derive(Debug, Copy, Clone)]
pub struct Token {
	/// Type of the token
	pub kind: TokenKind,    

	/// Source code location
	pub range: SourceRange, 
}

/// Abstract lexer
pub trait Lexer {
	/// Process source code and produce a stream of tokens
	fn process(&mut self, source: &str) -> Result<Vec<Token>, Token>;
}
