extern crate hdllang;
use hdllang::lexer::LogosLexer;
use hdllang::lexer::Lexer;

fn main() {
	let mut lexer = LogosLexer::new();
	match lexer.process("31 27 if > ; :") {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				println!("Token {:?}", t.kind);
			}
		},
		Err(_) => println!("lexer error!"),
	};
}
