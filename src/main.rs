extern crate hdllang;
use hdllang::{lexer::{LogosLexer, Lexer}, CompilerDiagnostic};

fn lexer_example() -> miette::Result<()> {
	let source = String::from(" 15_s4 0b11017 112y_u37 0xf1_s15 fun fun super_8  kdasd fun /* for */ aa bb aa 27  if ; 44 /**/  /*12 asd 34 56 4457 11 24 /**/  if ; // 44  11 ");
	let mut lexer = LogosLexer::new(&source);
	match lexer.process() {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				println!("Token {:?} - '{}'", t.kind, &source[t.range.start() .. t.range.end()]);
			}
		},
		Err(err) => {
			let diag = CompilerDiagnostic::from(err);
			Err(miette::Report::new(diag).with_source_code(source))?
		}
	};

	Ok(())
}

fn main() -> miette::Result<()> {
	lexer_example()?;
	Ok(())
}
