mod logos_lexer;
mod id_table;
use std::ops::Range;
pub use id_table::IdTable;
pub use logos_lexer::LogosLexer;

/// Lexer token type
/// In this case, it's defined by the Logos-based lexer implementation.
/// Not super pretty, but it's the simplest solution currently.
pub type TokenKind = logos_lexer::TokenKind;

/// All language keywords
#[derive(Debug, Clone, Copy)]
pub enum KeywordKind {
	Auto,
	Bus,
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
	LogiclalOr,      // ||
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
