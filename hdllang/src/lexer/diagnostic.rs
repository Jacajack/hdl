use crate::CompilerDiagnostic;
use super::LexerError;
use super::LexerErrorKind;
use super::numeric_constant_parser::NumericConstantParseErrorKind;

impl From<LexerError> for CompilerDiagnostic<LexerError> {
	fn from(err: LexerError) -> Self {
		let diag = CompilerDiagnostic::from_error(&err);
		match err.kind {

			LexerErrorKind::InvalidNumber(parse_error) => {
				let help;
				let label = "This is not a valid number";
				match parse_error.kind{
					NumericConstantParseErrorKind::BadBinaryDigit => 
						help = "1 and 0 are the only valid binary digits",

					NumericConstantParseErrorKind::BadDecimalDigit => 
						help = "0-9 are the only valid decimal digits",

					NumericConstantParseErrorKind::BadHexDigit => 
						help = "0-9, a-f and A-F are the only valid hexadecimal digits",

					NumericConstantParseErrorKind::InsufficientWidth => 
						help = "Width of this constant is insufficient to represent this number",

					NumericConstantParseErrorKind::TooManyBits => 
						help = "This numeric constant is too wide (too many bits)",

					NumericConstantParseErrorKind::WidthRequired => 
						help = "Please add width specifier - this constant exceeds plain integer range",
				};
				
				diag.label(err.range, label).help(help)
			}

			LexerErrorKind::UnterminatedBlockComment => diag
				.label(err.range, "This comment never ends")
				.help("Did you forget to use '*/"),

			LexerErrorKind::InvalidToken => diag
				.label(err.range, "This token doesn't make sense")
				.help("This is neither a keyword, an identifier nor a valid numeric constant"),
		}
	}
}
