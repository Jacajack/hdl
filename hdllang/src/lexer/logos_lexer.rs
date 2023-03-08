use logos::{Logos, Filter, Skip};
use super::{Lexer, SourceRange, Token, KeywordKind, PunctuatorKind};

/// Parses numeric constant strings
/// TODO proper implementation
/// ```
/// assert_eq!(parse_number_str("64"), 64);
/// assert_eq!(parse_number_str("112_u37", 112));
/// assert_eq!(parse_number_str("0xf_s11", 15));
/// assert_eq!(parse_number_str("0B11_01", 13));
/// ```
fn parse_number_str(s: &str) -> Option<u64>{
	match s.parse() {
		Ok(n) => Some(n),
		_ => None
	}
}

/// Causes lexer to consume and ignore multi-line comments (/* */)
fn consume_block_comment(lex: &mut logos::Lexer<TokenKind>) -> Filter<()> {
	match lex.remainder().find("*/") {
		Some(offset) => {lex.bump(offset + 2); Filter::Skip}
		None => Filter::Emit(())
	}
}

/// Causes lexer to consume and ignore single-line comments (//)
fn consume_line_comment(lex: &mut logos::Lexer<TokenKind>) -> Skip {
	match lex.remainder().find("\n") {
		Some(offset) => lex.bump(offset + 1),
		None => lex.bump(lex.remainder().len()),
	}
	Skip
}

#[derive(Logos, Debug, Clone, Copy)]
pub enum TokenKind {
	#[error]
	#[regex(r"[ \t\n\f]+", logos::skip)]
	#[token("/*", consume_block_comment)]
	#[token("//", consume_line_comment)]
	Error,

	// TODO constant table
	#[regex(r"((0[xX][\da-fA-F_]+)|(0[bB][10_]+)|(\d[\d_]*))([us]\d+)?", |lex| parse_number_str(lex.slice()))]
	Number(u64),

	// TODO symbol table
	// #[regex("[a-zA-Z_]+", |lex| String::from(lex.slice()))]
	// Id(String),

	#[token("if",  |_| KeywordKind::If)]
	#[token("for", |_| KeywordKind::For)]
	Keyword(KeywordKind),

	#[token("(", |_| PunctuatorKind::LPar)]
	#[token(")", |_| PunctuatorKind::RPar)]
	#[token("{", |_| PunctuatorKind::LBrace)]
	#[token("}", |_| PunctuatorKind::RBrace)]
	#[token("[", |_| PunctuatorKind::LBracket)]
	#[token("]", |_| PunctuatorKind::RBracket)]
	#[token(";", |_| PunctuatorKind::Semicolon)]
	#[token("+", |_| PunctuatorKind::Plus)]
	#[token("-", |_| PunctuatorKind::Minus)]
	#[token("/", |_| PunctuatorKind::Slash)]
	#[token("*", |_| PunctuatorKind::Asterisk)]
	#[token("%", |_| PunctuatorKind::Percent)]
	Punctuator(PunctuatorKind),
}

/// Logos-based lexer implementation
pub struct LogosLexer {
}

impl LogosLexer {
	pub fn new() -> LogosLexer {
		LogosLexer{}
	}
}

impl Lexer for LogosLexer {
	fn process(&mut self, source: &str) -> Result<Vec<Token>, Token> {
		// TODO determine average token length and pre-allocate vector space based on that
		let mut tokens = Vec::<Token>::with_capacity(source.len() / 10);
		let lexer = TokenKind::lexer(source);

		for (token_kind, range) in lexer.spanned() {
			let token = Token{
				kind: token_kind,
				range: SourceRange::new(&range),
			};
			
			if matches!(token.kind, TokenKind::Error) {
				return Err(token);
			}

			tokens.push(token);
		}

		Ok(tokens)
	}
}

#[cfg(tests)]
mod tests {
}