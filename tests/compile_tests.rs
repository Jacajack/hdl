extern crate hdllang;
use hdllang::compiler_diagnostic::ProvidesCompilerDiagnostic;
use hdllang::lexer::{IdTable, Lexer, LogosLexer, LogosLexerContext};
use hdllang::parser;
use hdllang::parser::ast::Root;
use hdllang::parser::ParserError;
use rstest::*;
use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use subprocess::{ExitStatus, Popen, PopenConfig};
use tempfile::NamedTempFile;

// Copied from main.rs
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

// Copied from main.rs
// It would be nice if we had this living in hdllang crate.
fn compile(mut code: String, file_name: String, output: &mut dyn Write) -> miette::Result<()> {
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
		&mut ctx.id_table,
		&ctx.numeric_constants,
		&root,
		String::from("."),
		&mut map,
	)
	.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code.clone())))?;
	hdllang::analyzer::SemanticalAnalyzer::new(global_ctx, &modules)
		.compile(output)
		.map_err(|e| e.with_source_code(miette::NamedSource::new(file_name.clone(), code)))?;
	Ok(())
}

fn run_hdlc(input_path: &Path) -> miette::Result<NamedTempFile> {
	let src = std::fs::read_to_string(input_path).expect("failed to read source code");
	let mut tmpfile = NamedTempFile::new().unwrap();
	compile(
		src,
		input_path
			.file_name()
			.expect("filename needed")
			.to_str()
			.unwrap()
			.into(),
		&mut tmpfile,
	)?;
	Ok(tmpfile)
}

fn run_iverilog(iverilog_path: &Path, input_path: &Path) -> Result<NamedTempFile, String> {
	let bin_file = NamedTempFile::new().unwrap();

	let mut p = Popen::create(
		&[
			iverilog_path.to_str().unwrap(),
			"-g2005-sv",
			"-Wall",
			"-o",
			bin_file.path().to_str().unwrap(),
			input_path.to_str().unwrap(),
		],
		PopenConfig::default(),
	)
	.expect("failed to spawn iverilog");

	use ExitStatus::*;
	match p.wait().unwrap() {
		Exited(0) => {},
		Exited(err) => return Err(format!("Exit code: {}", err)),
		Signaled(signum) => return Err(format!("Signaled: {}", signum)),
		Other(err) => return Err(format!("Other: {}", err)),
		Undetermined => panic!("cannot determine exit code"),
	}

	Ok(bin_file)
}

#[rstest]
fn test_compile_success(#[files("tests/input/*.hirl")] path: PathBuf) {
	let sv_file = run_hdlc(path.as_path()).unwrap();

	if !std::env::var("NO_IVERILOG").is_ok() {
		let iverilog_path = std::env::var("IVERILOG_PATH").unwrap_or("iverilog".into());
		match run_iverilog(Path::new(&iverilog_path), sv_file.path()) {
			Ok(_) => {},
			Err(e) => {
				eprintln!("iverilog failed: {}", e);
				eprintln!("Faulty SV codegen follows:");
				eprintln!("============================");
				eprintln!("{}", std::fs::read_to_string(sv_file.path()).unwrap());
				eprintln!("============================");
				panic!("iverilog failed");
			},
		}
	}
}

#[rstest]
fn test_compile_failure(#[files("tests/input_invalid/*.hirl")] path: PathBuf) {
	assert!(run_hdlc(path.as_path()).is_err());
}
