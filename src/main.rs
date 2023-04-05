extern crate hdllang;
use hdllang::CompilerDiagnostic;
use hdllang::parser;
use hdllang::lexer::{LogosLexer, Lexer, Token};
use miette::NamedSource;
use thiserror::Error;
use miette::{Diagnostic, SourceSpan};
use clap::{command,Arg,arg};
use std::io;
use std::fs;
use std::io::Write;
#[derive(Error, Diagnostic, Debug)]
pub enum PrettyIoError {
    #[error(transparent)]
    #[diagnostic(code(my_lib::io_error))]
    IoError(#[from] std::io::Error),
}

#[derive(Error, Debug, Diagnostic)]
#[error("Lexer error!")]
#[diagnostic(
	code(hdllang::lexer),
	url("patrzuwa.ga"),
	help("just use google bro")
)]
struct LexerErrorMessage {
	#[source_code]
	src: NamedSource,

	#[label("oopsie")]
	token_range: SourceSpan
}

impl LexerErrorMessage {
	pub fn new(source_name: &str, source: &str, token: &Token) -> LexerErrorMessage {
		LexerErrorMessage {
			src: NamedSource::new(String::from(source_name), String::from(source)),
			token_range: (token.range.start(), token.range.end() - token.range.start() + 1).into()
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
				println!("Token {:?} - '{}'", t.kind, &source[t.range.start() .. t.range.end()]);
			}
		},
		Err(err) => {
			let diag = CompilerDiagnostic::from(err);
			Err(miette::Report::new(diag).with_source_code(source))?
		}
	};

	Ok(())
}

fn simple_cmd(filename: String, mut output:Box<dyn Write>) -> miette::Result<()>{
	let content;
	match fs::read_to_string(&filename){
		Ok(file) =>{
			content = file;
		}
		Err(err) =>{
			return Err(PrettyIoError::IoError(err).into())
		}
	}
	let mut lexer = LogosLexer::new(&content);
	match lexer.process() {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				write!(output,"Token {:?} - '{}'\n", t.kind, &content[t.range.start() .. t.range.end()]);
			}
		},
		Err(token) => {
			let diag = CompilerDiagnostic::from(token);
			Err(miette::Report::new(diag).with_source_code(content))?
		}
	};
	Ok(())
}
fn main() ->  miette::Result<()> {
	let mut lex =LogosLexer::new("2*3+1");
	let expr = parser::ExprParser::new()
	.parse(lex)
	.unwrap();
	println!("{:?}", expr);
	/*
	let matches = command!() 
	.arg(Arg::new("source").required(true))
	.arg(Arg::new("output").short('o').long("output"))
	.arg(
		arg!(<MODE>)
			.help("Specify which action should be performed")
			.value_parser(["tokenize","parse","analyse","compile"])
			.required(false)
			.short('m')
			.long("mode")
	)
	.get_matches();

	let mode = match matches
		.get_one::<String>("MODE"){
			None=> "compile",
			Some(x) => x,
		};
	let output: Box<dyn Write> = match matches
		.get_one::<String>("output"){
			None=>Box::new(io::stdout()),
			Some(path)=> match fs::File::create(path){
				Ok(file) =>{
					Box::new(file)
				}
				Err(err) =>{
					return Err(PrettyIoError::IoError(err).into());
				}
			}
		};
	match mode{
		"tokenize"=>{
			let name = match matches.get_one::<String>("source"){
				Some(x)=>x,
				None=>"",
			};
			simple_cmd(name.to_owned(),output)?;
		},
		"parse"=>{
			println!("Not implemented!");
			lexer_example()?;
		},
		"analyse"=>{
			println!("Not implemented!");
			lexer_example()?;
		},
		"compile"=>{
			println!("Not implemented!");
			lexer_example()?;
		},
		_=>{
			println!("We wont be here");
			lexer_example()?;
		}
	};*/
	Ok(())
}
