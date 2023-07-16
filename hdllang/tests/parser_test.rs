
macro_rules!  parse_func{
	($name:ident, $production_name: ident) => {
		paste!{
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
mod import_path_parser_test{
	use paste::paste;
	parse_func!(import_path,ImportPathParser);

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
	fn simple(){
	    let s = "foo";
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
	#[test]
	fn import_single(){
		let s = "foo::bar::baz::qux";
		parse_import_path_pass(s);
	}
}
mod top_definition_parser_test{

	use paste::paste;
	parse_func!(top_definition,TopDefinitionParser);

	#[test]
	fn package_simple_import_path(){
		let s = "package foo;";
		parse_top_definition_pass(s);
	}
	
	#[test]
	fn use_import_path(){
		let s = "use foo::bar::baz;";
		parse_top_definition_pass(s);
	}

	#[test]
	fn use_missing_semicolon() {
		let s = "use foo::bar::baz";
		parse_top_definition_fail(s);
	}
	
	#[test]
	fn module_declaration_empty(){
		let s = "module foo{}";
		parse_top_definition_pass(s);
	}
	
	#[test]
	fn module_implementation_empty(){
		let s = "impl foo{}";
		parse_top_definition_pass(s);
	}
}
mod root_parser_test{

	use paste::paste;
	parse_func!(izulu,IzuluParser);

	#[test]
	fn pipelined_is_prime(){
		let s = "module pipelined_is_prime {
			input clock clk;
			input nreset;
		
			input in_valid;
			output in_ready;
			input bus<4> number;
		
			input out_ready;
			output sync(clk) {
				wire is_prime;
				wire out_valid;
			};
		}
		
		impl pipelined_is_prime {
			in_ready = out_ready; // comb passthrough - should warn
		
			register out_valid_r {
				clk, // must be a clock signal
				nreset,
				en: true,
				data: out_valid,
				next: in_valid,
			};
		
			register is_prime_r {
				clk,    // auto connected
				nreset, // auto connected
				en: out_ready,
				data: is_prime, // type deduced from input, connects to 'output sync(clk)'
				next: match(number) {
					2_u4, 3_u4, 5_u4, 7_u4, 11_u4, 13_u4 => true, // or 1u1
					default => false,
				},
			};
		}";
		parse_izulu_pass(s);
	}

	#[test]
	fn pipelined_division(){
		let s = "module full_adder {
			input a;
			input b;
			input cin;
			output cout;
			output q;
		}
		
		
		impl full_adder {
			cout = (a & b) | (cin & (a ^ b));
			q = a ^ b ^ cin;
		}
		
		module div_cell {
			input a;
			input b;
			input s;
			input cin;
			output cout;
			output r;
		}
		
		impl div_cell {
			full_adder u_fa{
				a,
				b: !b,
				cin,
				cout,
				q: auto fa_out
			};
		
			r = s ? fa_out : a;
		}
		
		module div_comb_stage {
			int WIDTH;
			input bus<WIDTH> a;
			input bus<WIDTH> b;
			input cin;
			output cout;
			output bus<WIDTH> r;
		}
		
		impl div_comb_stage {
			bus<WIDTH> cin_chain;
			bus<WIDTH> cout_chain;
		
			cin_chain[0] = cin;
			cout = cout_chain[WIDTH - 1];
			cin_chain[(WIDTH-1):1] = cout_chain[WIDTH-2:0];
			
			for (i in [0:WIDTH-1]) {
				div_cell u_cell{
					a: a[i],
					b: b[i],
					s: cout_chain[WIDTH - 1],
					r: r[i],
					cin: cin_chain[i],
					cout: cout_chain[i],
				}
			}
		}
		
		module comb_division {
			int A_WIDTH;
			int B_WIDTH;
			input<A_WIDTH> a;
			input<B_WIDTH> b;
			input<A_WIDTH> q;
			input<(B_WIDTH - 1)> r;
		}
		
		impl comb_division {
			auto STAGE_WIDTH = B_WIDTH + 1;
			bus<STAGE_WIDTH> stage_as[A_WIDTH];
			bus<STAGE_WIDTH> stage_b = zext(b);
			bus<STAGE_WIDTH> stage_outputs[A_WIDTH];
		
			stage_as[0] = zext(a[A_WIDTH - 1]);
			r = stage_outputs[A_WIDTH - 1][STAGE_WIDTH - 2:0];
		
			// Narrow stages
			for (i in [0:B_WIDTH - 1]) {
				div_comb_stage u_stage{
					WIDTH: B_WIDTH,
					a: trunc(stage_as[i]),
					b: trunc(stage_b),
					cin: true,
					cout: q[A_WIDTH - 1 - i],
					r: stage_outputs[i][STAGE_WIDTH - 2:0],
				}
			}
		
			// Wide stages
			for (i in [B_WIDTH:A_WIDTH - 1]) {
				div_comb_stage u_stage{
					WIDTH: B_WIDTH + 1,
					a: stage_as[i],
					b: stage_b,
					cin: true,
					cout: q[A_WIDTH - 1 - i],
					r: stage_outputs[i],
				}
			}
		
			// Conns
			for (i in [1:A_WIDTH - 1]) {
				stage_as[i][0] = a[A_WIDTH - 1 - i];
				stage_as[i][STAGE_WIDTH - 1:1] = stage_outputs[i - 1][STAGE_WIDTH - 2:0];
			}
		}";
		parse_izulu_pass(s);
	}

	// should we accept empty file?
	#[test]
	fn empty_file(){
		let s = "";
		parse_izulu_fail(s);
	}

	#[test]
	fn forbidden_keyword(){
		let s = "module module{}";
		parse_izulu_fail(s);
	}
}
