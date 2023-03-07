use hdllang::lexer::{Lexer, LogosLexer, TokenKind, KeywordKind};

#[test]
fn super_basic_lexer_test() {
	let mut lexer = LogosLexer::new();
	let source = "11 46 /* 56 */ foo if";
	let tokens = lexer.process(source).expect("Tokens expected");
	matches!(tokens[0].kind, TokenKind::Number(11));
	matches!(tokens[1].kind, TokenKind::Number(46));
	// matches!(tokens[2].kind, TokenKind::Id("foo")); // TODO symbol table
	matches!(tokens[3].kind, TokenKind::Keyword(KeywordKind::If));
	assert_eq!(tokens.len(), 4);
}