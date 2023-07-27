macro_rules! parse_func {
	($name:ident, $production_name: ident) => {
		paste! {
			fn [<parse_ $name>](source: &str, should_pass: bool) {
				use hdllang::core::DiagnosticBuffer;
				use hdllang::lexer::{Lexer, LogosLexer};
				use hdllang::parser::ParserContext;
				use hdllang::parser::$production_name;
				let lexer = LogosLexer::new(source);
				let buf = Box::new(DiagnosticBuffer::new());
				let mut ctx = ParserContext { diagnostic_buffer: buf };
				let result = $production_name::new()
					.parse(&mut ctx, Some(&String::from(source)), lexer);
				if should_pass{
					assert!(
						result.is_ok(),
						"\npath: {:?} is not ok,\nactual result was {:?}\n",
						source, result
					)
				} else {
					assert!(
						result.is_err(),
						"\npath: {:?} was parsed succesfully, but it should not,\nresult was {:?}\n",
						source, result
					)
				};
			}
			fn [<parse_ $name _pass>](source: &str) {
				[<parse_ $name>](source, true);
			}
			fn [<parse_ $name _fail>](source: &str) {
				[<parse_ $name>](source, false);
			}
		}
	};
}
mod import_path_parser_test {
	use paste::paste;
	parse_func!(import_path, ImportPathParser);

	#[test]
	fn with_super() {
		let s = "super::super::super::foo::{a, b, c}";
		parse_import_path_pass(s);
	}
	#[test]
	fn with_root() {
		let s = "root::foo::{a, b, c}";
		parse_import_path_pass(s);
	}
	#[test]
	fn with_implicit_root() {
		let s = "::foo::{a, b, c}";
		parse_import_path_pass(s);
	}
	#[test]
	fn local() {
		let s = "foo::jas::{a, b, c}";
		parse_import_path_pass(s);
	}

	#[test]
	fn simple() {
		let s = "foo";
		parse_import_path_pass(s);
	}

	#[test]
	fn root_inside_path() {
		let s = "foo::root::{a, b, c}";
		parse_import_path_fail(s);
	}
	#[test]
	fn with_super_inside_path() {
		let s = "foo::super::{a, b, c}";
		parse_import_path_fail(s);
	}
	#[test]
	fn with_implicit_root_inside_path() {
		let s = "foo::::{a, b, c}";
		parse_import_path_fail(s);
	}
	#[test]
	fn with_super_and_root_inside_path() {
		let s = "foo::super::root::{a, b, c}";
		parse_import_path_fail(s);
	}
	#[test]
	fn with_super_and_implicit_root_inside_path() {
		let s = "foo::super::::{a, b, c}";
		parse_import_path_fail(s);
	}
	#[test]
	fn import_all() {
		let s = "foo::bar::baz::*";
		parse_import_path_pass(s);
	}
	#[test]
	fn import_single() {
		let s = "foo::bar::baz::qux";
		parse_import_path_pass(s);
	}
}
mod top_definition_parser_test {

	use paste::paste;
	parse_func!(top_definition, TopDefinitionParser);

	#[test]
	fn package_simple_import_path() {
		let s = "package foo;";
		parse_top_definition_pass(s);
	}

	#[test]
	fn use_import_path() {
		let s = "use foo::bar::baz;";
		parse_top_definition_pass(s);
	}

	#[test]
	fn use_missing_semicolon() {
		let s = "use foo::bar::baz";
		parse_top_definition_fail(s);
	}

	#[test]
	fn module_declaration_empty() {
		let s = "module foo{}";
		parse_top_definition_pass(s);
	}

	#[test]
	fn module_implementation_empty() {
		let s = "impl foo{}";
		parse_top_definition_pass(s);
	}
}
mod root_parser_test {

	use paste::paste;
	parse_func!(izulu, IzuluParser);
	use std::fs;
	#[test]
	fn pipelined_is_prime() {
		let d = "tests_files/pipelined_is_prime.hirl";
		let s = fs::read_to_string(d.clone()).expect(format!("file {:?} could not be opened", d).as_str());
		parse_izulu_pass(s.as_str());
	}

	#[test]
	fn pipelined_division() {
		let d = "tests_files/pipelined_division.hirl";
		let s = fs::read_to_string(d.clone()).expect(d);
		parse_izulu_pass(s.as_str());
	}

	#[test]
	fn empty_file() {
		let s = "";
		parse_izulu_pass(s);
	}

	#[test]
	fn forbidden_keyword() {
		let s = "module module{}";
		parse_izulu_fail(s);
	}
}
