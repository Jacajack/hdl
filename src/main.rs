extern crate hdllang;
extern crate sha256;
use clap::{arg, command, Arg, ArgGroup};
use hdllang::compiler_diagnostic::ProvidesCompilerDiagnostic;
use hdllang::core::DiagnosticBuffer;
use hdllang::lexer::{IdTable, Lexer, LogosLexer, LogosLexerContext};
use hdllang::parser;
use hdllang::parser::ast::Root;
use hdllang::parser::pretty_printer::PrettyPrintable;
use hdllang::parser::ParserError;
use hdllang::serializer::SerializerContext;
use hdllang::CompilerDiagnostic;
use hdllang::CompilerError;
use log::{debug, info};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::io::Write;

fn lexer_example() -> miette::Result<()> {
	let source = String::from(" 15_s4 0b11017 112y_u37 0xf1_s15 fun fun super_8  kdasd fun /* for */ aa bb aa 27  if ; 44 /**/  /*12 asd 34 56 4457 11 24 /**/  if ; // 44  11 ");
	let mut lexer = LogosLexer::new(&source);
	match lexer.process() {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
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
fn read_input_from_file(filename: &String) -> miette::Result<String> {
	match fs::read_to_string(&filename) {
		Ok(file) => Ok(file),
		Err(_) => Err(CompilerError::FileNotFound(filename.to_string()).to_miette_report()),
	}
}
fn tokenize(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
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
fn parse(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let lexer = LogosLexer::new(&code);
	let buf = Box::new(hdllang::core::DiagnosticBuffer::new());
	let mut ctx = parser::ParserContext { diagnostic_buffer: buf };
	let parser = parser::IzuluParser::new();
	let ast = parser
		.parse(&mut ctx, Some(&code), lexer)
		.map_err(|e| ParserError::new_form_lalrpop_error(e).to_diagnostic())?;
	let buffer = ctx.diagnostic_buffer;
	println!("{}", buffer);
	write!(&mut output, "{:?}", ast).map_err(|e| CompilerError::IoError(e).to_miette_report())?;
	Ok(())
}

fn parse_file_recover_tables(
	code: String,
	ctx: LogosLexerContext,
) -> miette::Result<(Root, LogosLexerContext, String)> {
	let mut lexer = LogosLexer::new_with_context(&code, ctx);
	let buf = Box::new(hdllang::core::DiagnosticBuffer::new());
	let mut ctx = parser::ParserContext { diagnostic_buffer: buf };
	let parser = parser::IzuluParser::new();
	let ast = parser.parse(&mut ctx, Some(&code), &mut lexer).map_err(|e| {
		ParserError::new_form_lalrpop_error(e)
			.to_miette_report()
			.with_source_code(code.clone())
	})?;
	Ok((ast, lexer.get_context(), code))
}

fn serialize(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let mut lexer = LogosLexer::new(&code);
	let buf = Box::new(hdllang::core::DiagnosticBuffer::new());
	let mut ctx = parser::ParserContext { diagnostic_buffer: buf };
	let parser = parser::IzuluParser::new();
	let ast = parser.parse(&mut ctx, Some(&code), &mut lexer).map_err(|e| {
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
fn deserialize(code: String, output: Box<dyn Write>) -> miette::Result<()> {
	let deserialized: SerializerContext = serde_json::from_str(&code).unwrap();
	let mut printer = parser::pretty_printer::PrettyPrinterContext::new(
		&deserialized.id_table,
		&deserialized.comment_table,
		&deserialized.nc_table,
		output,
	);
	deserialized.ast_root.pretty_print(&mut printer)?;
	Ok(())
}
fn pretty_print(code: String, output: Box<dyn Write>) -> miette::Result<()> {
	let mut lexer = LogosLexer::new(&code);
	let buf = Box::new(DiagnosticBuffer::new());
	let mut ctx = parser::ParserContext { diagnostic_buffer: buf };
	let ast = parser::IzuluParser::new()
		.parse(&mut ctx, Some(&code), &mut lexer)
		.map_err(|e| {
			ParserError::new_form_lalrpop_error(e)
				.to_miette_report()
				.with_source_code(code.clone())
		})?;
	let buffer = ctx.diagnostic_buffer;
	println!("{}", buffer);
	let mut printer = parser::pretty_printer::PrettyPrinterContext::new(
		lexer.id_table(),
		lexer.comment_table(),
		lexer.numeric_constant_table(),
		output,
	);
	ast.pretty_print(&mut printer)?;
	Ok(())
}
fn init_logging() {
	if let Ok(logfile) = std::env::var("RUST_LOG_FILE") {
		// See: https://github.com/rust-cli/env_logger/issues/125
		env_logger::Builder::from_default_env()
			.target(env_logger::Target::Pipe(Box::new(
				std::fs::File::create(&logfile).expect("Could not create LOG_FILE"),
			)))
			.init();

		info!("Logging to file '{}'", logfile);
	}
	else {
		env_logger::init();
		info!("Hello! Logging to stderr...");
	}
}

fn combine(root_file_name: String, mut output: Box<dyn Write>) -> miette::Result<()> {
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
		comment_table: hdllang::lexer::CommentTable::new(),
		numeric_constants: hdllang::lexer::NumericConstantTable::new(),
		last_err: None,
	};
	let source: String;
	while let Some(file_name) = file_queue.pop_front() {
		let current_directory = Path::new(&file_name).parent().unwrap().to_str().unwrap();
		debug!("Current directory: {}", current_directory);
		let code = read_input_from_file(&file_name)?;
		(root, ctx, source) = parse_file_recover_tables(code, ctx)?;
		let name = Path::new(&file_name).to_str().unwrap().to_string();
		let (paths, ..) = hdllang::analyzer::combine(
			&ctx.id_table,
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
fn compile(mut code: String, file_name: String, mut output: Box<dyn Write>) -> miette::Result<()>{
	let root: Root;
	let mut ctx = LogosLexerContext {
		id_table: IdTable::new(),
		comment_table: hdllang::lexer::CommentTable::new(),
		numeric_constants: hdllang::lexer::NumericConstantTable::new(),
		last_err: None,
	};
	let mut map: HashMap<String, String> = HashMap::new();
	(root, ctx, code) = parse_file_recover_tables(code, ctx)?;
	let (_, global_ctx, modules) = hdllang::analyzer::combine(
		&ctx.id_table,
		&ctx.numeric_constants,
		&root,
		String::from("."),
		&mut map,
	)
	.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code.clone())))?;
	// analyse semantically
	hdllang::analyzer::SemanticalAnalyzer::new(global_ctx, &modules)
		.process()
		.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name, code)))?;
	//hdllang::analyzer::analyze_semantically(&mut global_ctx, &modules)?;
	writeln!(output, "{}", "semantical analysis was perfomed succesfully")
		.map_err(|e: io::Error| CompilerError::IoError(e).to_diagnostic())?;
	Ok(())
}
fn analyse(mut code: String, file_name: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	// tokenize and parse
	let root: Root;
	let mut ctx = LogosLexerContext {
		id_table: IdTable::new(),
		comment_table: hdllang::lexer::CommentTable::new(),
		numeric_constants: hdllang::lexer::NumericConstantTable::new(),
		last_err: None,
	};
	let mut map: HashMap<String, String> = HashMap::new();
	(root, ctx, code) = parse_file_recover_tables(code, ctx)?;
	let (_, global_ctx, modules) = hdllang::analyzer::combine(
		&ctx.id_table,
		&ctx.numeric_constants,
		&root,
		String::from("."),
		&mut map,
	)
	.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code.clone())))?;
	// analyse semantically
	hdllang::analyzer::SemanticalAnalyzer::new(global_ctx, &modules)
		.process()
		.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name, code)))?;
	//hdllang::analyzer::analyze_semantically(&mut global_ctx, &modules)?;
	writeln!(output, "{}", "semantical analysis was perfomed succesfully")
		.map_err(|e: io::Error| CompilerError::IoError(e).to_diagnostic())?;
	Ok(())
}
fn main() -> miette::Result<()> {
	//std::env::set_var("RUST_LOG", "info");
	//std::env::set_var("RUST_BACKTRACE", "1");
	init_logging();

	let matches = command!()
		.arg(Arg::new("source"))
		.arg(Arg::new("output").short('o').long("output"))
		.arg(arg!(-c --clean "Clean build files"))
		.arg(
			arg!(<MODE>)
				.help("Specify which action should be performed")
				.value_parser([
					"tokenize",
					"tokenise",
					"parse",
					"pretty-print",
					"serialize",
					"serialise",
					"deserialize",
					"deserialise",
					"analyse",
					"analyze",
					"combine", // for development only
					"compile",
				])
				.required(false)
				.short('m')
				.long("mode"),
		)
		.group(
			ArgGroup::new("clean_or_source")
				.args(["source", "clean"])
				.required(true),
		)
		.group(ArgGroup::new("mode_or_clean").args(["MODE", "clean"]).required(false))
		.get_matches();

	let mut mode = match matches.get_one::<String>("MODE") {
		None => "compile",
		Some(x) => x,
	};
	match matches.get_one::<bool>("clean") {
		Some(x) => {
			if *x {
				mode = "clean";
			}
		},
		None => (),
	};
	let output: Box<dyn Write> = match matches.get_one::<String>("output") {
		None => Box::new(io::stdout()),
		Some(path) => match fs::File::create(path) {
			Ok(file) => Box::new(file),
			Err(err) => {
				return Err(CompilerError::IoError(err).to_miette_report())?;
			},
		},
	};
	let file_name = match matches.get_one::<String>("source") {
		Some(x) => x,
		None => "",
	};
	let code = match mode {
		"clean" | "combine" => "".to_string(),
		_ => read_input_from_file(&String::from(file_name))?,
	};
	match mode {
		"tokenize" | "tokenise" => {
			tokenize(code, output)?;
		},
		"parse" => {
			parse(code, output)?;
		},
		"combine" => {
			combine(String::from(file_name), output)?;
		},
		"analyze" | "analyse" => {
			analyse(code, String::from(file_name), output)?;
		},
		"pretty-print" => {
			pretty_print(code, output)?;
		},
		"serialize" | "serialise" => {
			serialize(code, output)?;
		},
		"deserialize" | "deserialise" => {
			deserialize(code, output)?;
		},
		"compile" => {
			compile(code, String::from(file_name), output)?;
		},
		"clean" => {
			println!("Not implemented!");
			println!("Cleaning of build files was requested!")
		},
		_ => lexer_example()?,
	};
	Ok(())
}
