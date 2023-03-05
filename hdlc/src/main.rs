extern crate hdllang;
use hdllang::lexer::LogosLexer;
use hdllang::lexer::Lexer;

fn main() {
	let mut lexer = LogosLexer::new();
	match lexer.process("module test{}") {
		Ok(tokens) => println!("okayy we have {} tokens", tokens.len()),
		Err(_) => println!("lexer error!"),
	};
}
