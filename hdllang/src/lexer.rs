mod logos_lexer;
mod id_table;
pub use logos_lexer::LogosLexer;
use std::ops::Range;

use self::id_table::IdTable;

/// Lexer token type
/// In this case, it's defined by the Logos-based lexer implementation.
/// Not super pretty, but it's the simplest solution currently.
pub type TokenKind = logos_lexer::TokenKind;

/// All language keywords
#[derive(Debug, Clone, Copy)]
pub enum KeywordKind {
	Module,
	If,
	For,
	Register,
	Input,
	Output,
	Wire,
	Bus,
	Sync,
	Clock,
	Conditional,
	Match,
	Comb,
	Tristate,
	Int,
	Signed,
	Unsigned,
	Auto,
	Unused,
	Const,
	FfSync,
	ClockGate,
	TristateBuffer,
	Enum
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
pub trait Lexer<'source> {
	/// Creates a lexer for provided source code
	fn new(source: &'source str) -> Self;

	/// Processes the text and returns a vector of tokens
	fn process(&mut self) -> Result<Vec<Token>, Token>;

	/// Access the ID table
	fn id_table(&self) -> &IdTable;
}
