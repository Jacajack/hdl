use crate::compiler_diagnostic::ProvidesCompilerDiagnostic;
use crate::lexer::{IdTable, Lexer, LogosLexer, LogosLexerContext};
use crate::parser;
use crate::parser::ast::Root;
use crate::parser::pretty_printer::PrettyPrintable;
use crate::parser::ParserError;
use crate::serializer::SerializerContext;
use crate::CompilerDiagnostic;
use crate::CompilerError;
use log::{debug, info};
use std::collections::HashMap;
use std::fs;
use std::io;
use std::io::Write;

/// This function is used to present lexer example
pub fn lexer_example() -> miette::Result<()> {
	let source = String::from(" 15_s4 0b11017 112y_u37 0xf1_s15 fun fun super_8  kdasd fun /* for */ aa bb aa 27  if ; 44 /**/  /*12 asd 34 56 4457 11 24 /**/  if ; // 44  11 ");
	let mut lexer = LogosLexer::new(&source);
	match lexer.process() {
		Ok(tokens) => {
			println!(
				"{} tokens have been succesfully extracted from the source code",
				tokens.len()
			);
			for t in &tokens {
				println!("Token {:?} - '{}'", t.kind, &source[t.range.start()..t.range.end()]);
			}
		},
		Err(err) => {
			let diag = CompilerDiagnostic::from(err);
			Err(miette::Report::new(diag).with_source_code(source))?
		},
	};

	Ok(())
}
/// This function is used to read input from file, it provides the filename if the file is not found, contrary to the standard library
pub fn read_input_from_file(filename: &String) -> miette::Result<String> {
	match fs::read_to_string(&filename) {
		Ok(file) => Ok(file),
		Err(_) => Err(CompilerError::FileNotFound(filename.to_string()).to_miette_report()),
	}
}

/// This function is used to tokenize the source code
pub fn tokenize(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let mut lexer = LogosLexer::new(&code);
	match lexer.process() {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				writeln!(
					output,
					"Token {:?} - '{}'",
					t.kind,
					&code[t.range.start()..t.range.end()]
				)
				.map_err(|e| CompilerError::IoError(e).to_miette_report())?
			}
		},
		Err(err) => Err(err.to_miette_report().with_source_code(code))?,
	};
	Ok(())
}
/// This function is used to tokenize and parse the source code
pub fn parse(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let lexer = LogosLexer::new(&code);
	let parser = parser::IzuluParser::new();
	let ast = parser
		.parse(Some(&code), lexer)
		.map_err(|e| ParserError::new_form_lalrpop_error(e).to_diagnostic())?;
	write!(&mut output, "{:?}", ast).map_err(|e| CompilerError::IoError(e).to_miette_report())?;
	Ok(())
}

pub fn parse_file_recover_tables(
	code: String,
	ctx: LogosLexerContext,
) -> miette::Result<(Root, LogosLexerContext, String)> {
	let mut lexer = LogosLexer::new_with_context(&code, ctx);
	let parser = parser::IzuluParser::new();
	let ast = parser.parse(Some(&code), &mut lexer).map_err(|e| {
		ParserError::new_form_lalrpop_error(e)
			.to_miette_report()
			.with_source_code(code.clone())
	})?;
	Ok((ast, lexer.get_context(), code))
}

pub fn serialize(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let mut lexer = LogosLexer::new(&code);
	let parser = parser::IzuluParser::new();
	let ast = parser.parse(Some(&code), &mut lexer).map_err(|e| {
		ParserError::new_form_lalrpop_error(e)
			.to_miette_report()
			.with_source_code(code.clone())
	})?;
	let serializer_ctx = SerializerContext {
		ast_root: ast,
		id_table: lexer.id_table().clone(),
		comment_table: lexer.comment_table().clone(),
		nc_table: lexer.numeric_constant_table().clone(),
	};
	let res =
		serde_json::to_string_pretty(&serializer_ctx).map_err(|err| CompilerError::JsonError(err).to_diagnostic())?;
	writeln!(output, "{}", res).map_err(|e: io::Error| CompilerError::IoError(e).to_diagnostic())?;
	Ok(())
}
pub fn deserialize(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let deserialized: SerializerContext = serde_json::from_str(&code).unwrap();
	let mut printer = parser::pretty_printer::PrettyPrinterContext::new(
		&deserialized.id_table,
		&deserialized.comment_table,
		&deserialized.nc_table,
		&mut output,
	);
	deserialized.ast_root.pretty_print(&mut printer)?;
	Ok(())
}
pub fn pretty_print(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let mut lexer = LogosLexer::new(&code);
	let ast = parser::IzuluParser::new().parse(Some(&code), &mut lexer).map_err(|e| {
		ParserError::new_form_lalrpop_error(e)
			.to_miette_report()
			.with_source_code(code.clone())
	})?;
	let mut printer = parser::pretty_printer::PrettyPrinterContext::new(
		lexer.id_table(),
		lexer.comment_table(),
		lexer.numeric_constant_table(),
		&mut output,
	);
	ast.pretty_print(&mut printer)?;
	Ok(())
}

pub fn combine(root_file_name: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	use std::path::Path;
	let target_directory = "hirn_target/intermidiate";
	match Path::new(target_directory).exists() {
		true => (),
		false => fs::create_dir_all(target_directory).map_err(|err| CompilerError::IoError(err).to_miette_report())?,
	};
	use std::collections::VecDeque;
	let mut file_queue: VecDeque<String> = VecDeque::from([root_file_name.clone()]);
	debug!("File queue: {:?}", file_queue);
	use sha256::try_digest;
	let hash = try_digest(root_file_name.clone())
		.map_err(|_| CompilerError::FileNotFound(root_file_name.clone()).to_miette_report())?;
	let mut map = HashMap::new();
	map.insert(hash, String::from("root"));

	// tokenize and parse
	let root: Root;
	let mut ctx = LogosLexerContext {
		id_table: IdTable::new(),
		comment_table: crate::lexer::CommentTable::new(),
		numeric_constants: crate::lexer::NumericConstantTable::new(),
		last_err: None,
	};
	let source: String;
	while let Some(file_name) = file_queue.pop_front() {
		let current_directory = Path::new(&file_name).parent().unwrap().to_str().unwrap();
		debug!("Current directory: {}", current_directory);
		let code = read_input_from_file(&file_name)?;
		(root, ctx, source) = parse_file_recover_tables(code, ctx)?;
		let name = Path::new(&file_name).to_str().unwrap().to_string();
		let (paths, ..) = crate::analyzer::combine(
			&mut ctx.id_table,
			&ctx.numeric_constants,
			&root,
			String::from(current_directory),
			&mut map,
		)
		.map_err(|e| e.with_source_code(miette::NamedSource::new(name, source)))?;
		for path in paths {
			file_queue.push_back(path);
		}
		println!("File queue: {:?}", file_queue);
		break; // we stop after the first file for now
	}
	writeln!(output, "{}", "done").map_err(|e: io::Error| CompilerError::IoError(e).to_diagnostic())?;
	Ok(())
}
pub fn compile(mut code: String, file_name: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let root: Root;
	let mut ctx = LogosLexerContext {
		id_table: IdTable::new(),
		comment_table: crate::lexer::CommentTable::new(),
		numeric_constants: crate::lexer::NumericConstantTable::new(),
		last_err: None,
	};
	let mut map: HashMap<String, String> = HashMap::new();
	(root, ctx, code) = parse_file_recover_tables(code, ctx)?;
	let (_, global_ctx, modules) = crate::analyzer::combine(
		&mut ctx.id_table,
		&ctx.numeric_constants,
		&root,
		String::from("."),
		&mut map,
	)
	.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code.clone())))?;
	// analyse semantically
	let mut analyzer = crate::analyzer::SemanticalAnalyzer::new(global_ctx, &modules);
	analyzer
		.compile(&mut *output)
		.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code.clone())))?;
	for diag in analyzer.buffer().buffer {
		println!(
			"{:?}",
			miette::Report::new(diag).with_source_code(miette::NamedSource::new(file_name.clone(), code.clone()))
		)
	}
	//crate::analyzer::analyze_semantically(&mut global_ctx, &modules)?;
	info!("File {} compiled succesfully", file_name);
	Ok(())
}
pub fn elaborate(mut code: String, file_name: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let root: Root;
	let mut ctx = LogosLexerContext {
		id_table: IdTable::new(),
		comment_table: crate::lexer::CommentTable::new(),
		numeric_constants: crate::lexer::NumericConstantTable::new(),
		last_err: None,
	};
	let mut map: HashMap<String, String> = HashMap::new();
	(root, ctx, code) = parse_file_recover_tables(code, ctx)?;
	let (_, global_ctx, modules) = crate::analyzer::combine(
		&mut ctx.id_table,
		&ctx.numeric_constants,
		&root,
		String::from("."),
		&mut map,
	)
	.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code.clone())))?;
	// analyse semantically
	let mut analyzer = crate::analyzer::SemanticalAnalyzer::new(global_ctx, &modules);
	analyzer
		.compile_and_elaborate(&mut *output)
		.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code.clone())))?;
	for diag in analyzer.buffer().buffer {
		println!(
			"{:?}",
			miette::Report::new(diag).with_source_code(miette::NamedSource::new(file_name.clone(), code.clone()))
		)
	}
	info!("File {} compiled and elaborated succesfully", file_name);
	Ok(())
}
pub fn analyse(mut code: String, file_name: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	// tokenize and parse
	let root: Root;
	let mut ctx = LogosLexerContext {
		id_table: IdTable::new(),
		comment_table: crate::lexer::CommentTable::new(),
		numeric_constants: crate::lexer::NumericConstantTable::new(),
		last_err: None,
	};
	let mut map: HashMap<String, String> = HashMap::new();
	(root, ctx, code) = parse_file_recover_tables(code, ctx)?;
	let (_, global_ctx, modules) = crate::analyzer::combine(
		&mut ctx.id_table,
		&ctx.numeric_constants,
		&root,
		String::from("."),
		&mut map,
	)
	.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code.clone())))?;
	// analyse semantically
	crate::analyzer::SemanticalAnalyzer::new(global_ctx, &modules)
		.semantical_analysis()
		.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name, code)))?;
	//crate::analyzer::analyze_semantically(&mut global_ctx, &modules)?;
	writeln!(output, "{}", "Semantical analysis was perfomed succesfully")
		.map_err(|e: io::Error| CompilerError::IoError(e).to_diagnostic())?;
	Ok(())
}
