extern crate hdllang;
use hdllang::lexer::LogosLexer;
use hdllang::lexer::Lexer;

fn main() {
	let mut lexer = LogosLexer::new();
	let source = "31 for 27 test if > ; 44 /**/ */ /*12 34*/ 56 4457 11 24 /* if ; // 44 \n 11";
	match lexer.process(source) {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				println!("Token {:?} - '{}'", t.kind, &source[t.range.start .. t.range.end]);
			}
		},
		Err(_) => println!("lexer error!"),
	};
}
