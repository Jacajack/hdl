use logos::{Logos, Filter, Skip};
use super::id_table::{IdTable, IdTableKey};
use super::number_parser::parse_number_str;
use super::{Lexer, SourceSpan, Token, KeywordKind, PunctuatorKind, LexerError, LexerErrorKind};

/// Parses numeric constant tokens
fn parse_number_token(lex: &mut logos::Lexer<TokenKind>) -> Option<u64> {
	parse_number_str(lex.slice()).map_err(|err| {
		lex.extras.last_err = Some(LexerError{
			range: SourceSpan::new_from_range(&lex.span()), // TODO fix this span
			kind: LexerErrorKind::InvalidNumber(err),
		});
		return ();
	}).ok()
}

/// Causes lexer to consume and ignore multi-line comments (/* */)
fn consume_block_comment(lex: &mut logos::Lexer<TokenKind>) -> Filter<()> {
	match lex.remainder().find("*/") {
		Some(offset) => {lex.bump(offset + 2); Filter::Skip}
		None => {
			let span = lex.span();
			lex.extras.last_err = Some(LexerError{
				range: SourceSpan::new(
					span.start,
					lex.remainder().len() + span.end - span.start
				),
				kind: LexerErrorKind::UnterminatedBlockComment,
			});
			Filter::Emit(())
		}
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
	#[regex(r"[ \t\n\f]+", logos::skip)]
	#[token("/*", consume_block_comment)]
	#[token("//", consume_line_comment)]
	Error,

	// TODO constant table
	#[regex(r"[0-9][a-zA-Z0-9_]*", parse_number_token)]
	Number(u64),

	#[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", register_id_token)]
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
	#[token("%", |_| PunctuatorKind::Modulo)]
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
	last_err: Option<LexerError>,
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
					id_table: IdTable::new(),
					last_err: None,
				}
			)
		}
	}
	
	/// Processes the string and produces a vector of tokens
	fn process(&mut self) -> Result<Vec<Token>, LexerError> {
		// TODO determine average token length and pre-allocate vector space based on that
		let mut tokens = Vec::<Token>::with_capacity(1000);
		while let Some(token_kind) = self.lexer.next() {
			let token = Token{
				kind: token_kind,
				range: SourceSpan::new_from_range(&self.lexer.span()),
			};
			
			// Early return with the error reported by parsing functions
			// or a generic one
			if matches!(token.kind, TokenKind::Error) {
				return Err(self.lexer.extras.last_err.unwrap_or(LexerError{
					range: token.range,
					kind: LexerErrorKind::InvalidToken,
				}));
			}

			tokens.push(token);
		}

		// Successful exit
		assert!(matches!(self.lexer.extras.last_err, None));
		Ok(tokens)
	}

	/// Provides access to the ID table
	fn id_table(&self) -> &IdTable {
		&self.lexer.extras.id_table
	}
}
