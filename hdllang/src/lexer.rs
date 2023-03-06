use crate::symtable::SymbolKey;
use logos::{Logos, Filter, Skip};

// TODO token or struct?
pub struct LexerError {
	
}

pub struct SourceLocation {
	pub offset: usize,
	pub size: usize,
}

#[derive(Debug)]
pub enum KeywordType {
	If,
	For,
	Wire,
	Signed,
	Unsigned,
}

/// Parses number token
fn parse_token_number(lex: &mut logos::Lexer<TokenKind>) -> Option<u64> {
	match lex.slice().parse() {
		Ok(s) => Some(s),
		Err(_) => None
	}
}

fn parse_token_keyword(lex: &mut logos::Lexer<TokenKind>) -> Option<KeywordType> {
	match lex.slice() {
		"if" => Some(KeywordType::If),
		_ => None
	}
}

fn parse_block_comment(lex: &mut logos::Lexer<TokenKind>) -> Filter<()> {
	match lex.remainder().find("*/") {
		Some(offset) => {lex.bump(offset + 2); Filter::Skip}
		None => Filter::Emit(())
	}
}

fn parse_line_comment(lex: &mut logos::Lexer<TokenKind>) -> Skip {
	match lex.remainder().find("\n") {
		Some(offset) => lex.bump(offset + 1),
		None => lex.bump(lex.remainder().len()),
	}
	Skip
}

#[derive(Logos, Debug)]
pub enum TokenKind {
	#[error]
	#[regex(r"[ \t\n\f]+", logos::skip)]
	#[token("/*", parse_block_comment)]
	#[token("//", parse_line_comment)]
	Error,

	#[regex(r"[0-9]+", parse_token_number)]
	Number(u64),

	// #[regex("[a-zA-Z_]+")]
	// Id(SymbolKey),

	#[regex("[a-zA-Z_]+", parse_token_keyword)]
	Keyword(KeywordType),

	#[token("(")]
	LPar,

	#[token(")")]
	RPar,

	#[token("{")]
	LBrace,

	#[token("}")]
	RBrace,

	#[token("[")]
	LBracket,

	#[token("]")]
	RBracket,

	#[token(";")]
	Semicolon,

	#[token("+")]
	Plus,

	#[token("-")]
	Minus,

	#[token("..")]
	Concat,

	#[token("/")]
	Divide,

	#[token("*")]
	Multiply,

	#[token("%")]
	Modulo,
}

pub struct Token {
	pub kind: TokenKind,
	pub location: SourceLocation
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
		let mut lexer = TokenKind::lexer(source);
		let mut tokens = Vec::<Token>::new();

		while let Some(token_kind) = lexer.next() {
			tokens.push(Token{
				kind: token_kind,
				location: SourceLocation {
					offset: 0,
					size: 0
				}
			});
		}

		Ok(tokens)
	}
}