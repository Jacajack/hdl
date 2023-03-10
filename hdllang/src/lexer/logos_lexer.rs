use logos::{Logos, Filter, Skip};
use super::{Lexer, SourceRange, Token, KeywordKind, PunctuatorKind};

/// Parses numeric constant strings
/// TODO proper implementation
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

	#[token("module",          |_| KeywordKind::Module)]
	#[token("register",        |_| KeywordKind::Register)]
	#[token("input",           |_| KeywordKind::Input)]
	#[token("output",          |_| KeywordKind::Output)]
	#[token("wire",            |_| KeywordKind::Wire)]
	#[token("sync",            |_| KeywordKind::Sync)]
	#[token("clock",           |_| KeywordKind::Clock)]
	#[token("conditional",     |_| KeywordKind::Conditional)]
	#[token("match",           |_| KeywordKind::Match)]
	#[token("bus",             |_| KeywordKind::Bus)]
	#[token("comb",            |_| KeywordKind::Comb)]
	#[token("tristate",        |_| KeywordKind::Tristate)]
	#[token("int",             |_| KeywordKind::Int)]
	#[token("signed",          |_| KeywordKind::Signed)]
	#[token("unsigned",        |_| KeywordKind::Unsigned)]
	#[token("auto",            |_| KeywordKind::Auto)]
	#[token("unused",          |_| KeywordKind::Unused)]
	#[token("const",           |_| KeywordKind::Const)]
	#[token("ff_sync",         |_| KeywordKind::FfSync)]
	#[token("clock_gate",      |_| KeywordKind::ClockGate)]
	#[token("tristate_buffer", |_| KeywordKind::TristateBuffer)]
	#[token("enum",            |_| KeywordKind::Enum)]
	#[token("if",              |_| KeywordKind::If)]
	#[token("for",             |_| KeywordKind::For)]
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

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_number_token() {
		assert_eq!(parse_number_str("64"), Some(64));
		assert_eq!(parse_number_str("112_u37"), Some(112));
		assert_eq!(parse_number_str("0xf_s11"), Some(15));
		assert_eq!(parse_number_str("0B11_01"), Some(13));
		assert_eq!(parse_number_str("_17"), None);
		assert_eq!(parse_number_str("xffa"), None);
	}
}