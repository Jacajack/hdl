use logos::{Logos, Filter, Skip};
use super::id_table::{IdTable, IdTableKey};
use super::{Lexer, SourceRange, Token, KeywordKind, PunctuatorKind};

// TODO maybe we should be moving out parse_xxx functions out of here?

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

/// Registers token in the lexer's ID table
fn register_id_token(lex: &mut logos::Lexer<TokenKind>) -> IdTableKey {
	lex.extras.id_table.insert_or_get(lex.slice())
}

#[derive(Logos, Debug, Clone, Copy)]
#[logos(extras = LogosLexerContext)]
pub enum TokenKind{
	#[error]
	#[regex(r"[ \t\r\n\f]+", logos::skip)]
	#[token("/*", consume_block_comment)]
	#[token("//", consume_line_comment)]
	Error,

	// TODO constant table
	#[regex(r"((0[xX][\da-fA-F_]+)|(0[bB][10_]+)|(\d[\d_]*))([us]\d+)?", |lex| parse_number_str(lex.slice()))]
	Number(u64),

	#[regex("[a-zA-Z_]+", register_id_token)]
	Id(IdTableKey),

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

/// Additional data structures accessed by the lexers
/// 
/// This struct is not contained in LogosLexer, but is
/// passed as an extra to logos::Lexer and hence owned
/// by it.
pub struct LogosLexerContext {
	/// Identifier table (names only)
	id_table: IdTable,
}

/// Logos-based lexer implementation
pub struct LogosLexer<'source> {
	lexer: logos::Lexer<'source, TokenKind>,
}

/// Lexer implementation based on logos <3
impl<'source> Lexer<'source> for LogosLexer<'source> {
	/// Creates a new lexer given a source code string
	fn new(source: &'source str) -> Self {
		LogosLexer{
			lexer: TokenKind::lexer_with_extras(
				source,
				LogosLexerContext{
					id_table: IdTable::new()
				}
			)
		}
	}
	
	/// Processes the string and produces a vector of tokens
	fn process(&mut self) -> Result<Vec<Token>, Token> {
		// TODO determine average token length and pre-allocate vector space based on that
		let mut tokens = Vec::<Token>::with_capacity(1000);
		while let Some(token_kind) = self.lexer.next() {
			let token = Token{
				kind: token_kind,
				range: SourceRange::new(&self.lexer.span()),
			};
			
			if matches!(token.kind, TokenKind::Error) {
				return Err(token);
			}

			tokens.push(token);
		}

		Ok(tokens)
	}

	/// Provides access to the ID table
	fn id_table(&self) -> &IdTable {
		&self.lexer.extras.id_table
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