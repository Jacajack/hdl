use crate::CompilerDiagnostic;
use crate::ProvidesCompilerDiagnostic;
use super::LexerError;
use super::LexerErrorKind;
use super::NumberParseError;
use super::number_parser::NumberParseErrorKind;

impl ProvidesCompilerDiagnostic for NumberParseError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		let diag = CompilerDiagnostic::from_error(&self);
		let help;
		let label = "This is not a valid number";
		match self.kind {
			NumberParseErrorKind::BadBinaryDigit => 
				help = "1 and 0 are the only valid binary digits",

			NumberParseErrorKind::BadDecimalDigit => 
				help = "0-9 are the only valid decimal digits",

			NumberParseErrorKind::BadHexDigit => 
				help = "0-9, a-f and A-F are the only valid hexadecimal digits",

			NumberParseErrorKind::InsufficientWidth => 
				help = "Width of this constant is insufficient to represent this number",

			NumberParseErrorKind::TooManyBits => 
				help = "This numeric constant is too wide (too many bits)",

			NumberParseErrorKind::WidthRequired => 
				help = "Please add width specifier - this constant exceeds plain integer range",
		};
			
		diag.label(self.range.into(), label).help(help)
	}

}

impl ProvidesCompilerDiagnostic for LexerError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		match self.kind {
			LexerErrorKind::InvalidNumber(parse_err) => 
				parse_err.into(),

			LexerErrorKind::UnterminatedBlockComment =>
				CompilerDiagnostic::from_error(&self)
				.label(self.range, "This comment never ends")
				.help("Did you forget to use '*/"),

			LexerErrorKind::InvalidToken =>
				CompilerDiagnostic::from_error(&self)
				.label(self.range, "This token doesn't make sense")
				.help("This is neither a keyword, an identifier nor a valid numeric constant"),
		}
	}
}
