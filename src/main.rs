extern crate hdllang;
use hdllang::lexer::{LogosLexer, Lexer, Token};
use miette::NamedSource;
use thiserror::Error;
use miette::{Diagnostic, SourceSpan};

#[derive(Error, Debug, Diagnostic)]
#[error("Lexer error!")]
#[diagnostic(
	code(hdllang::lexer),
	url("patrzuwa.ga"),
	help("git gud")
)]
struct LexerErrorMessage {
	#[source_code]
	src: NamedSource,

	#[label("What is this anyway?")]
	token_range: SourceSpan
}

impl LexerErrorMessage {
	pub fn new(source: &str, token: &Token) -> LexerErrorMessage {
		LexerErrorMessage {
			src: NamedSource::new("idk.lol", String::from(source)),
			token_range: (token.range.start, token.range.end - token.range.start + 1).into()
		}
	}
}

fn lexer_example() -> miette::Result<()> {
	let mut lexer = LogosLexer::new();
	let source = String::from("31 for 27  if ; 44 /**/ */ /*12 asd 34*/ 56 4457 11 24 /* */ if ; // 44 \n 11 ");
	match lexer.process(&source) {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				println!("Token {:?} - '{}'", t.kind, &source[t.range.start .. t.range.end]);
			}
		},
		Err(token) => {
			return Err(LexerErrorMessage::new(&source, &token))?
		}
	};

	Ok(())
}

fn main() -> miette::Result<()> {
	lexer_example()?;
	Ok(())
}
