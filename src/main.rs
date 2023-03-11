extern crate hdllang;

use anyhow::Ok;
use hdllang::lexer::{LogosLexer, Lexer, Token};
use miette::NamedSource;
use thiserror::Error;
use miette::{Diagnostic, SourceSpan};
use clap::{command,Arg};

use std::fs;

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
			token_range: (token.range.start, token.range.end - token.range.start + 1).into()
		}
	}
}

fn lexer_example() -> miette::Result<()> {
	let source = String::from("31 fun fun fun for aa bb aa 27  if ; 44 /**/ */ /*12 asd 34*/ 56 4457 11 24 /* */ if ; // 44 \n 11 ");
	let mut lexer = LogosLexer::new(&source);
	match lexer.process() {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				println!("Token {:?} - '{}'", t.kind, &source[t.range.start .. t.range.end]);
			}
		},
		Err(token) => {
			return Err(LexerErrorMessage::new("example", &source, &token))?
		}
	};

	Ok(())
}

fn simple_cmd(filename: String) -> miette::Result<()>{
	let content;
	match fs::read_to_string(&filename){
		Ok(file) =>{
			content = file;
		}
		Err(err) =>{
			return Err(PrettyIoError::IoError(err).into())
		}
	}
	let mut lexer = LogosLexer::new();
	match lexer.process(&content) {
		Ok(tokens) => {
			println!("okayy we have {} tokens", tokens.len());
			for t in &tokens {
				println!("Token {:?} - '{}'", t.kind, &content[t.range.start .. t.range.end]);
			}
		},
		Err(token) => {
			return Err(LexerErrorMessage::new(&filename,&content, &token))?
		}
	};
	Ok(())
}

fn main() ->  miette::Result<()> {
	let matches = command!() 
	.arg(Arg::new("source").short('s').long("source").required(true))
	.arg(Arg::new("output").short('o').long("output"))
	.arg(
		arg!(<MODE>)
			.help("Specify which action should be performed")
			.value_parser(["--tokenize","--parse","--analyse","--compile"])
			.required(true)
	)
	.get_matches();

	let mode = match matches
		.get_one::<String>("MODE"){
			None=> "--compile",
			Some(x) => x,
		};
	match mode{
		"--tokenize"=>{},
		"--parse"=>{},
		"--analyse"=>{},
		"--compile"=>{},
	};
	// let args: Vec<String> = std::env::args().collect();
	// match args.len(){
	// 	// no args passed
	// 	1=>{
	// 		lexer_example()?;
	// 	},
	// 	// two arguments passed
	// 	3=>{
	// 		match args[1].as_str(){
	// 			"--tokenize"=>{
	// 				simple_cmd(args[2].to_string())?;
	// 			},
	// 			"--help"=>{
	// 				println!("Use '--tokenize <filename>' to tokenize the input");
	// 			}
	// 			_=>{
	// 				println!("Use --help to get help")
	// 			}
	// 		}
	// 	},
	// 	_=>{}
	// }
	// Ok(())
}
