mod import_path_parser_test{
use hdllang::{self};
fn parse_import_path_pass(source: &str) {
	use hdllang::core::DiagnosticBuffer;
	use hdllang::lexer::{Lexer, LogosLexer};
	use hdllang::parser::ParserContext;
	use hdllang::parser::ImportPathParser;
	let lexer = LogosLexer::new(source);
	let buf = Box::new(DiagnosticBuffer::new());
	let mut ctx = ParserContext { diagnostic_buffer: buf };
	let result = ImportPathParser::new()
		.parse(&mut ctx, Some(&String::from(source)), lexer);
	assert!(
		result.is_ok(),
		"\npath: {:?} is not ok,\nactual result was {:?}\n", 
		source, result
	);
}
fn parse_import_path_fail(source: &str) {
	use hdllang::core::DiagnosticBuffer;
	use hdllang::lexer::{Lexer, LogosLexer};
	use hdllang::parser::ParserContext;
	use hdllang::parser::ImportPathParser;
	let lexer = LogosLexer::new(source);
	let buf = Box::new(DiagnosticBuffer::new());
	let mut ctx = ParserContext { diagnostic_buffer: buf };
	let result = ImportPathParser::new()
		.parse(&mut ctx, Some(&String::from(source)), lexer);
	assert!(
		result.is_err(),
		"\npath: {:?} was parsed succesfully, but it should not,\nresult was {:?}\n", 
		source, result
	);
}
#[test]
fn with_super(){
    let s = "super::super::super::foo::{a, b, c}";
    parse_import_path_pass(s);
}
#[test]
fn with_root(){
    let s = "root::foo::{a, b, c}";
    parse_import_path_pass(s);
}
#[test]
fn with_implicit_root(){
    let s = "::foo::{a, b, c}";
	parse_import_path_pass(s);
}
#[test]
fn local(){
    let s = "foo::jas::{a, b, c}";
    parse_import_path_pass(s);
}
#[test]
fn root_inside_path(){
    let s = "foo::root::{a, b, c}";
    parse_import_path_fail(s);
}
#[test]
fn with_super_inside_path(){
	let s = "foo::super::{a, b, c}";
	parse_import_path_fail(s);
}
#[test]
fn with_implicit_root_inside_path(){
	let s = "foo::::{a, b, c}";
	parse_import_path_fail(s);
}
#[test]
fn with_super_and_root_inside_path(){
	let s = "foo::super::root::{a, b, c}";
	parse_import_path_fail(s);
}
#[test]
fn with_super_and_implicit_root_inside_path(){
	let s = "foo::super::::{a, b, c}";
	parse_import_path_fail(s);
}
#[test]
fn import_all(){
	let s = "foo::bar::baz::*";
	parse_import_path_pass(s);
}
}