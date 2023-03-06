extern crate hdllang;
use hdllang::lexer::LogosLexer;
use hdllang::lexer::Lexer;

fn main() {
	let mut lexer = LogosLexer::new();
	match lexer.process("31 27 if > ; 44 /**/ */ /*12 34*/ 56 4457 11 24 /* if ; // 44 \n 11") {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				println!("Token {:?}", t.kind);
			}
		},
		Err(_) => println!("lexer error!"),
	};
}
