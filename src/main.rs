extern crate hdllang;
use hdllang::lexer::{LogosLexer, Lexer, LexerError, LexerErrorKind, NumberParseError};
use miette::NamedSource;
use thiserror::Error;
use miette::{Diagnostic, SourceSpan};

// TODO move all these pretty error things into a separate module
// perhaps even inside hdllang
#[derive(Error, Debug, Diagnostic)]
#[error("Invalid token found!")]
enum LexerErrorMessage {

	#[error("Invalid token found")]
	#[diagnostic(
		code(hdllang::lexer),
		help("This is neither a keyword, an identifier nor a valid numeric constant")
	)]
	InvalidToken{
		#[source_code]
		src: NamedSource,

		#[label("This is the invalid token")]
		token_range: SourceSpan
	},

	#[error("Invalid numeric constant")]
	#[diagnostic(
		code(hdllang::lexer),
		help("This is not a valid numeric constant.")
	)]
	InvalidNumber{
		#[source_code]
		src: NamedSource,

		#[label("This thing here is not a valid number")]
		token_range: SourceSpan,
	},

	#[error("Unterminated block comment")]
	#[diagnostic(
		code(hdllang::lexer),
		help("Did you forget to use '*/'?")
	)]
	UnterminatedBlockComment{
		#[source_code]
		src: NamedSource,

		#[label("Look! It never ends!")]
		token_range: SourceSpan
	}
	
}

impl LexerErrorMessage {
	pub fn new(source_name: &str, source: &str, err: &LexerError) -> LexerErrorMessage {
		let span : SourceSpan = (err.range.start, err.range.end - err.range.start).into();
		let named_source = NamedSource::new(
			String::from(source_name),
			String::from(source)
		);


		match err.kind {
			LexerErrorKind::InvalidNumber(err) => LexerErrorMessage::InvalidNumber {
				src: named_source,
				token_range: span,
			},

			LexerErrorKind::UnterminatedBlockComment => LexerErrorMessage::UnterminatedBlockComment {
				src: named_source,
				token_range: span,
			},

			_ => LexerErrorMessage::InvalidToken {
				src: named_source,
				token_range: span,
			},
		}
	}
}

fn lexer_example() -> miette::Result<()> {
	let source = String::from(" 112_u37 0xf1_s15 fun fun super_8  kdasd fun /* for */ aa bb aa 27  if ; 44 /**/  /*12 asd 34 56 4457 11 24 /**/  if ; // 44  11 ");
	let mut lexer = LogosLexer::new(&source);
	match lexer.process() {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				println!("Token {:?} - '{}'", t.kind, &source[t.range.start .. t.range.end]);
			}
		},
		Err(err) => {
			return Err(LexerErrorMessage::new("example.hdl", &source, &err))?
		}
	};

	Ok(())
}

fn main() -> miette::Result<()> {
	lexer_example()?;
	Ok(())
}
