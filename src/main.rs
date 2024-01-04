extern crate hdllang;
extern crate sha256;

use clap::{arg, command, Arg};
use hdllang::CompilerError;
use hdllang::ProvidesCompilerDiagnostic;
use std::fs;
use std::io::Write;

fn init_logging() {
	use log::*;
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

fn main() -> miette::Result<()> {
	init_logging();

	use hdllang::utils::*;

	let matches = command!()
		.arg(
			Arg::new("source")
			.help("Specify the name of the input file")
			.num_args(1)
			.required(true))
		.arg(
			Arg::new("output")
				.short('o')
				.long("output")
				.help("Specify the name of the output file")
				.num_args(1)
				.required(true),
		)
		//.arg(arg!(-c --clean "Clean build files"))
		.arg(
			Arg::new("json-report")
				.long("json-report")
				.help("Prints all reports in JSON format")
				.action(clap::ArgAction::SetTrue),
		)
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
					"combine",   // for development only
					"elaborate", // for development only
					"compile",
				])
				.required(false)
				.short('m')
				.long("mode"),
		)
		//.group(
		//	ArgGroup::new("clean_or_source")
		//		.args(["source", "clean"])
		//		.required(true),
		//)
		//.group(ArgGroup::new("mode_or_clean").args(["MODE", "clean"]).required(false))
		.get_matches();

	let mode = match matches.get_one::<String>("MODE") {
		None => "elaborate",
		Some(x) => x,
	};
	let json_report = matches.get_flag("json-report");
	if json_report {
		miette::set_hook(Box::new(|_| Box::new(miette::JSONReportHandler::new()))).unwrap();
	}
	//match matches.get_one::<bool>("clean") {
	//	Some(x) => {
	//		if *x {
	//			mode = "clean";
	//		}
	//	},
	//	None => (),
	//};
	let file_name = match matches.get_one::<String>("source") {
		Some(x) => x,
		None => "",
	};
	let mut output: Box<dyn Write> = match matches.get_one::<String>("output") {
		None => Box::new(std::io::stdout()),
		Some(path) => match fs::File::create(path) {
			Ok(file) => Box::new(file),
			Err(err) => {
				return Err(CompilerError::IoError(err).to_miette_report())?;
			},
		},
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
			pretty_print(code, &mut output)?;
		},
		"serialize" | "serialise" => {
			serialize(code, output)?;
		},
		"deserialize" | "deserialise" => {
			deserialize(code, output)?;
		},
		"compile" => {
			elaborate(code, String::from(file_name), &mut output, json_report, true)?;
		},
		"elaborate" => {
			elaborate(code, String::from(file_name), &mut output, json_report, true)?;
		},
		"clean" => {
			println!("Not implemented!");
			println!("Cleaning of build files was requested!")
		},
		_ => lexer_example()?,
	};
	Ok(())
}
