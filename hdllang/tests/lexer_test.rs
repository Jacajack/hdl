use hdllang::lexer::{Lexer, LogosLexer, TokenKind, KeywordKind};

#[test]
fn super_basic_lexer_test() {
	let source = "11 46 /* 56 */ if";
	let mut lexer = LogosLexer::new(source);
	let tokens = lexer.process().expect("Tokens expected");
	matches!(tokens[0].kind, TokenKind::Number(11));
	matches!(tokens[1].kind, TokenKind::Number(46));
	matches!(tokens[2].kind, TokenKind::Keyword(KeywordKind::If));
	assert_eq!(tokens.len(), 3);
}