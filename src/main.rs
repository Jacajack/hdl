extern crate hdllang;
use clap::{arg, command, Arg};
use hdllang::compiler_diagnostic::ProvidesCompilerDiagnostic;
use hdllang::core::DiagnosticBuffer;
use hdllang::lexer::{Lexer, LogosLexer};
use hdllang::CompilerDiagnostic;
use hdllang::CompilerError;
use hdllang::{analyzer, parser};
use log::info;
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
fn read_input_from_file(filename: String) -> miette::Result<String> {
	return match fs::read_to_string(&filename) {
		Ok(file) => Ok(file),
		Err(err) => return Err(CompilerError::IoError(err).to_miette_report()),
	};
}
fn tokenize(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let mut lexer = LogosLexer::new(&code);
	match lexer.process() {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				write!(
					output,
					"Token {:?} - '{}'\n",
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
	let mut ctx = parser::ParserContext {
		diagnostic_buffer: buf,
	};
	let parser = parser::IzuluParser::new();
	let ast = parser.parse(&mut ctx, Some(&code),lexer);
	let buffer = ctx.diagnostic_buffer;
	println!("{}",buffer.to_string());
	write!(&mut output, "{:?}", ast).map_err(|e| CompilerError::IoError(e).to_miette_report())?;
	Ok(())
}

fn analyze(code: String, mut output: Box<dyn Write>) -> miette::Result<()> {
	let mut lexer = LogosLexer::new(&code);
	let buf = Box::new(DiagnosticBuffer::new());
	let mut ctx = parser::ParserContext {
		diagnostic_buffer: buf,
	};
	let ast = parser::IzuluParser::new().parse(&mut ctx, Some(&code),&mut lexer);
	println!("Ids: {:?}", lexer.id_table());
	println!("Comments: {:?}", lexer.comment_table());
	let id_table = lexer.id_table().clone();
	let comment_table = lexer.comment_table().clone();

	let mut analyzer = analyzer::SemanticAnalyzer::new(&id_table, &comment_table);

	writeln!(&mut output, "{:?}", ast).map_err(|e| CompilerError::IoError(e).to_diagnostic())?;
	analyzer.process(ast.as_ref().unwrap());
	Ok(())
}

fn init_logging() {
	if let Some(logfile) = std::env::var("RUST_LOG_FILE").ok() {
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

fn main() -> miette::Result<()> {
	init_logging();

	let matches = command!()
		.arg(Arg::new("source").required(true))
		.arg(Arg::new("output").short('o').long("output"))
		.arg(
			arg!(<MODE>)
				.help("Specify which action should be performed")
				.value_parser(["tokenize", "parse", "analyse", "analyze", "compile"])
				.required(false)
				.short('m')
				.long("mode"),
		)
		.get_matches();

	let mode = match matches.get_one::<String>("MODE") {
		None => "compile",
		Some(x) => x,
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
	let name = match matches.get_one::<String>("source") {
		Some(x) => x,
		None => "",
	};
	let code = read_input_from_file(String::from(name))?;
	match mode {
		"tokenize" => {
			tokenize(code, output)?;
		},
		"parse" => {
			parse(code, output)?;
		},
		"analyse" | "analyze" => {
			analyze(code, output)?;
		},
		"compile" => {
			println!("Not implemented!");
			lexer_example()?;
		},
		_ => unreachable!(),
	};
	Ok(())
}
