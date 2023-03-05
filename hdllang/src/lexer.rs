use crate::symtable::SymbolKey;
use logos::Logos;

// TODO token or struct?
pub struct LexerError {
	
}

pub enum KeywordType {
	If,
	For,
	Wire,
	Signed,
	Unsigned,
}

/**
	This obviously doesn't make any sense now. This is just a code draft.
*/
pub enum Token {
	Number(u64),
	Keyword(KeywordType),
	Id(SymbolKey),
	LPar,
	RPar,
	LBrace,
	RBrace,
	LBracket,
	RBracket,
	Semicolon,
	Plus,
	Minus,
	Concat,
	Divide,
	Multiply,
	Modulo,
}

pub trait Lexer {
	fn process(&mut self, source: &str) -> Result<Vec<Token>, LexerError>;
}

pub struct LogosLexer {
}

impl LogosLexer {
	pub fn new() -> LogosLexer {
		LogosLexer{}
	}
}

impl Lexer for LogosLexer {
	fn process(&mut self, source: &str) -> Result<Vec<Token>, LexerError> {
		Err(LexerError{})
	}
}