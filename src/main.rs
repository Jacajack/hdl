extern crate hdllang;
//use anyhow::Ok;
use clap::{arg, command, Arg};
use hdllang::lexer::{Lexer, LogosLexer, Token};
use hdllang::parser;
use hdllang::CompilerDiagnostic;
use hdllang::CompilerError;
use log::info;
use miette::NamedSource;
use miette::{Diagnostic, SourceSpan};
use std::fs;
use std::io;
use std::io::Write;
use std::env;
use thiserror::Error;
use hdllang::compiler_diagnostic::ProvidesCompilerDiagnostic;
#[derive(Error, Diagnostic, Debug)]
pub enum PrettyIoError {
    #[error(transparent)]
    #[diagnostic(code(my_lib::io_error))]
    IoError(#[from] std::io::Error),
}

#[derive(Error, Debug, Diagnostic)]
#[error("Lexer error!")]
#[diagnostic(code(hdllang::lexer), url("patrzuwa.ga"), help("just use google bro"))]
struct LexerErrorMessage {
    #[source_code]
    src: NamedSource,

    #[label("oopsie")]
    token_range: SourceSpan,
}

impl LexerErrorMessage {
    pub fn _new(source_name: &str, source: &str, token: &Token) -> LexerErrorMessage {
        LexerErrorMessage {
            src: NamedSource::new(String::from(source_name), String::from(source)),
            token_range: (
                token.range.start(),
                token.range.end() - token.range.start() + 1,
            )
                .into(),
        }
    }
}

fn lexer_example() -> miette::Result<()> {
    let source = String::from(" 15_s4 0b11017 112y_u37 0xf1_s15 fun fun super_8  kdasd fun /* for */ aa bb aa 27  if ; 44 /**/  /*12 asd 34 56 4457 11 24 /**/  if ; // 44  11 ");
    let mut lexer = LogosLexer::new(&source);
    match lexer.process() {
        Ok(tokens) => {
            println!("okayy we have {} tokens", tokens.len());
            for t in &tokens {
                println!(
                    "Token {:?} - '{}'",
                    t.kind,
                    &source[t.range.start()..t.range.end()]
                );
            }
        }
        Err(err) => {
            let diag = CompilerDiagnostic::from(err);
            Err(miette::Report::new(diag).with_source_code(source))?
        }
    };

    Ok(())
}
fn read_input_from_file(filename: String) ->miette::Result<String>{
    return match fs::read_to_string(&filename) {
        Ok(file) => {
            Ok(file)
        }
        Err(err) => return Err(PrettyIoError::IoError(err).into()),
    }
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
                .map_err(|e| CompilerError::IoError(e).to_report())?
            }
        }
        Err(token) => {
            let diag = CompilerDiagnostic::from(token);
            Err(miette::Report::new(diag).with_source_code(code))?
        }
    };
    Ok(())
}
fn parse(code:String,mut output: Box<dyn Write>)-> miette::Result<()>{
    let lexer = LogosLexer::new(&code);
    let expr = parser::IzuluParser::new().parse(lexer);
    write!(&mut output,"{:?}",expr)
    .map_err(|e| CompilerError::IoError(e).to_report())?;
    Ok(())
}

fn init_logging() {
    if let Some(logfile) = std::env::var("RUST_LOG_FILE").ok() {
		// See: https://github.com/rust-cli/env_logger/issues/125
        env_logger::Builder::from_default_env()
            .target(
				env_logger::Target::Pipe(
					Box::new(
						std::fs::File::create(&logfile).expect("Could not create LOG_FILE")
					)
				)
			)
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
                .value_parser(["tokenize", "parse", "analyse", "compile"])
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
                return Err(PrettyIoError::IoError(err).into());
            }
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
        }
        "parse" => {
            parse(code,output)?;
        }
        "analyse" => {
            println!("Not implemented!");
            lexer_example()?;
        }
        "compile" => {
            println!("Not implemented!");
            lexer_example()?;
        }
        _ => unreachable!()
    };
    Ok(())
}
