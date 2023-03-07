use logos::{Logos, Filter, Skip};
use std::ops::Range;

// TODO token or struct?
#[derive(Debug)]
pub struct LexerError {
	
}

pub struct SourceLocation {
	pub offset: usize,
	pub size: usize,
}

#[derive(Debug)]
pub enum KeywordKind {
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

	#[regex("[a-zA-Z_]+", |lex| String::from(lex.slice()))]
	Id(String),

	#[token("if",  |_| KeywordKind::If)]
	#[token("for", |_| KeywordKind::For)]
	Keyword(KeywordKind),

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
	pub range: Range<usize>,
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
		let lexer = TokenKind::lexer(source);
		let mut tokens = Vec::<Token>::new();

		for (token_kind, range) in lexer.spanned() {
			tokens.push(Token{
				kind: token_kind,
				range: range,
			});
		}

		Ok(tokens)
	}
}