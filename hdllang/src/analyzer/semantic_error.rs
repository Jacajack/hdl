use crate::CompilerDiagnostic;
use thiserror::Error;

// #[derive(Clone, Copy, Debug, Error, Display)]
// #[error("Module has more than one `impl` block reffering to it")]
pub struct MultipleModuleImplementationsError {

}

#[derive(Copy, Clone, Error, Debug)]
pub enum SemanticError {
	#[error("Module has more than one `impl` block reffering to it")]
	MultipleModuleImplementations,
}


// impl From<SemanticError> for CompilerDiagnostic<SemanticError> {
// 	fn from(err: SemanticError) -> Self {
// 		let diag = CompilerDiagnostic::from_error(&err);
// 		// match err.kind {

// 		// 	LexerErrorKind::InvalidNumber(parse_error) => {
// 		// 		let help;
// 		// 		let label = "This is not a valid number";
// 		// 		match parse_error.kind{
// 		// 			NumberParseErrorKind::BadBinaryDigit => 
// 		// 				help = "1 and 0 are the only valid binary digits",

// 		// 			NumberParseErrorKind::BadDecimalDigit => 
// 		// 				help = "0-9 are the only valid decimal digits",

// 		// 			NumberParseErrorKind::BadHexDigit => 
// 		// 				help = "0-9, a-f and A-F are the only valid hexadecimal digits",

// 		// 			NumberParseErrorKind::InsufficientWidth => 
// 		// 				help = "Width of this constant is insufficient to represent this number",

// 		// 			NumberParseErrorKind::TooManyBits => 
// 		// 				help = "This numeric constant is too wide (too many bits)",

// 		// 			NumberParseErrorKind::WidthRequired => 
// 		// 				help = "Please add width specifier - this constant exceeds plain integer range",
// 		// 		};
				
// 		// 		diag.label(err.range, label).help(help)
// 		// 	}

// 		// 	LexerErrorKind::UnterminatedBlockComment => diag
// 		// 		.label(err.range, "This comment never ends")
// 		// 		.help("Did you forget to use '*/"),

// 		// 	LexerErrorKind::InvalidToken => diag
// 		// 		.label(err.range, "This token doesn't make sense")
// 		// 		.help("This is neither a keyword, an identifier nor a valid numeric constant"),
// 		// }
// 	}
// }
