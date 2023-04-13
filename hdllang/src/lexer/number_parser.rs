use crate::compiler_diagnostic::*;
use std::fmt;
use thiserror::Error;

/// Describes error types encountered when parsing strings
#[derive(Clone, Copy, Debug, Error)]
pub enum NumberParseErrorKind {
	#[error("0 and 1 are the only valid binary digits")]
	BadBinaryDigit,

	#[error("0-9 are the only valid decimal digits")]
	BadDecimalDigit,

	#[error("0-9, a-f and A-F are the only valid hexadecimal digits")]
	BadHexDigit,

	#[error("Insufficient width")]
	InsufficientWidth,

	#[error("Too many bits")]
	TooManyBits,

	#[error("Width specifier required - integer type overflow")]
	WidthRequired,
}

/// Describes number parsing error (type + location within string)
#[derive(Clone, Copy, Debug, Error)]
pub struct NumberParseError {
	pub kind: NumberParseErrorKind,
	pub range: (usize, usize), // TODO a common source range type
}

impl fmt::Display for NumberParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.kind)
	}
}

impl ProvidesCompilerDiagnostic for NumberParseError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		use NumberParseErrorKind::*;
		CompilerDiagnosticBuilder::from_error(&self)
			.label(self.range.into(), "This is not a valid number")
			.help(match self.kind {
				BadBinaryDigit => "1 and 0 are the only valid binary digits",
				BadDecimalDigit => "0-9 are the only valid decimal digits",
				BadHexDigit => "0-9, a-f and A-F are the only valid hexadecimal digits",
				InsufficientWidth => {
					"Width of this constant is insufficient to represent this number"
				}
				TooManyBits => "This numeric constant is too wide (too many bits)",
				WidthRequired => {
					"Please add width specifier - this constant exceeds plain integer range"
				}
			})
			.build()
	}
}

/// Parses a pure decimal number
fn parse_pure_decimal(s: &str) -> Result<u64, NumberParseError> {
	u64::from_str_radix(s, 10).map_err(|_| NumberParseError {
		kind: NumberParseErrorKind::BadDecimalDigit,
		range: (0, s.len()),
	})
}

/// Parses a pure hex number
fn parse_pure_hex(s: &str) -> Result<u64, NumberParseError> {
	u64::from_str_radix(s, 16).map_err(|_| NumberParseError {
		kind: NumberParseErrorKind::BadHexDigit,
		range: (0, s.len()),
	})
}

/// Parses a pure binary number
fn parse_pure_binary(s: &str) -> Result<u64, NumberParseError> {
	u64::from_str_radix(s, 2).map_err(|_| NumberParseError {
		kind: NumberParseErrorKind::BadBinaryDigit,
		range: (0, s.len()),
	})
}

/// Parses numeric constant strings
/// TODO wider return type
pub fn parse_number_str(s: &str) -> Result<u64, NumberParseError> {
	// These two should be enforced by the regex in the lexer
	assert!(s.len() > 0);
	assert!(s.chars().nth(0).unwrap().is_digit(10));

	// Get rid of all '_' and convert to lowercase
	let s = String::from(s).replace("_", "").to_lowercase();

	let mut is_signed: bool = true;
	let mut num_bits = 64u32;
	let mut digits_end = s.len();

	// Find the last character which could indicate signedness
	if let Some(index) = s.rfind("s").or(s.rfind("u")) {
		is_signed = s.chars().nth(index).unwrap() == 's';
		digits_end = index;

		let width_str = &s[index + 1..];
		if width_str.len() > 0 {
			num_bits = parse_pure_decimal(width_str)?.try_into().unwrap();
		}
	}

	// Check if number of bits is reasonable
	if num_bits > 64 {
		return Err(NumberParseError {
			kind: NumberParseErrorKind::TooManyBits,
			range: (0, s.len()),
		});
	}

	// TODO corner case - should oldest bit in binary representation affect sign?

	// Parse according to base
	let value = if s.starts_with("0x") {
		parse_pure_hex(&s[2..digits_end])?
	} else if s.starts_with("0b") {
		parse_pure_binary(&s[2..digits_end])?
	} else {
		parse_pure_decimal(&s[0..digits_end])?
	};

	// Check if the number can be represented with this number of bits
	let effective_bits = num_bits - if is_signed { 1 } else { 0 };
	if value > u64::pow(2u64, effective_bits) - 1 {
		return Err(NumberParseError {
			kind: NumberParseErrorKind::InsufficientWidth,
			range: (0, s.len()),
		});
	}

	Ok(value)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_number_token() {
		assert!(matches!(parse_number_str("64"), Ok(64)));
		assert!(matches!(parse_number_str("112_u37"), Ok(112)));
		assert!(matches!(parse_number_str("0xf_s11"), Ok(15)));
		assert!(matches!(parse_number_str("0B11_01"), Ok(13)));
		assert!(matches!(parse_number_str("1ffa"), Err(_)));
	}

	// TODO more tests
}
