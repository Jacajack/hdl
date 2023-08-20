use crate::{compiler_diagnostic::*, lexer::numeric_constant::NumericConstantBase};
use std::fmt;
use log::debug;
use num_bigint::BigInt;
use thiserror::Error;

use super::NumericConstant;

/// Describes error types encountered when parsing strings
#[derive(Clone, Copy, Debug, Error)]
pub enum NumericConstantParseErrorKind {
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
	pub kind: NumericConstantParseErrorKind,
	pub range: (usize, usize), // TODO a common source range type
}

impl fmt::Display for NumberParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.kind)
	}
}

impl ProvidesCompilerDiagnostic for NumberParseError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		use NumericConstantParseErrorKind::*;
		CompilerDiagnosticBuilder::from_error(&self)
			.label(self.range.into(), "This is not a valid number")
			.help(match self.kind {
				BadBinaryDigit => "1 and 0 are the only valid binary digits",
				BadDecimalDigit => "0-9 are the only valid decimal digits",
				BadHexDigit => "0-9, a-f and A-F are the only valid hexadecimal digits",
				InsufficientWidth => "Width of this constant is insufficient to represent this number",
				TooManyBits => "This numeric constant is too wide (too many bits)",
				WidthRequired => "Please add width specifier - this constant exceeds plain integer range",
			})
			.build()
	}
}

/// Parses a pure decimal number
fn parse_pure_decimal(s: &str) -> Result<BigInt, NumberParseError> {
	BigInt::parse_bytes(s.as_bytes(), 10).map_or_else(||
		Err(NumberParseError {
			kind: NumericConstantParseErrorKind::BadBinaryDigit,
			range: (0, s.len()),
		}), |f|{
			debug!("dec: {:?}, bigint: {:?}",s, f);
			Ok(f)
		})
}

/// Parses a pure hex number
fn parse_pure_hex(s: &str) -> Result<BigInt, NumberParseError> {
	BigInt::parse_bytes(s.as_bytes(), 16).map_or_else(||
		Err(NumberParseError {
			kind: NumericConstantParseErrorKind::BadBinaryDigit,
			range: (0, s.len()),
		}), |f|{
			debug!("hex: 0x{:?}, bigint: {:?}",s, f);
			Ok(f)
		})
}

/// Parses a pure binary number
fn parse_pure_binary(s: &str) -> Result<BigInt, NumberParseError> {
	BigInt::parse_bytes(s.as_bytes(), 2).map_or_else(||
		Err(NumberParseError {
			kind: NumericConstantParseErrorKind::BadBinaryDigit,
			range: (0, s.len()),
		}), |f|{
			debug!("bin: 0b{:?}, bigint: {:?}",s, f);
			Ok(f)
		})
}

fn parse_two_complement(s: &str)-> Result<String, NumberParseError>{
	let mut  new = String::new();
	for char in s.chars(){
		match char{
			'0' => new.push('1'),
			'1' => new.push('0'),
			_ =>  return Err(NumberParseError {
				kind: NumericConstantParseErrorKind::BadBinaryDigit,
				range: (0, s.len()),
			}),
		}
	}
	Ok(new)
}
/// Parses numeric constant strings
pub fn parse_numeric_constant_str(s: &str) -> Result<NumericConstant, NumberParseError> {
	// These two should be enforced by the regex in the lexer
	assert!(!s.is_empty());
	assert!(s.chars().next().unwrap().is_ascii_digit());

	// Token width before we remove underscores
	let token_len = s.len();

	// Get rid of all '_' and convert to lowercase
	let s = String::from(s).replace('_', "").to_lowercase();

	let mut is_signed: Option<bool> = None;
	let mut num_bits: Option<u32> = None;
	let mut digits_end = s.len();

	// Find the last character which could indicate signedness
	if let Some(index) = s.rfind('s').or(s.rfind('u')) {
		is_signed = Some(s.chars().nth(index).unwrap() == 's');
		digits_end = index;

		let width_str = &s[index + 1..];
		if !width_str.is_empty() {
			let n = parse_pure_decimal(width_str)?.try_into().unwrap();

			// Is the number of bits reasonable?
			if n > 64 {
				return Err(NumberParseError {
					kind: NumericConstantParseErrorKind::TooManyBits,
					range: (0, token_len),
				});
			}

			num_bits = Some(n);
		}
	}

	// TODO corner case - should oldest bit in binary representation affect sign?
	// TODO same dillema but for hex numbers
	// TODO handle precise error underline in funky numbers like 0b__1__2__u35

	// Parse according to base
	let base;
	let value = if s.starts_with("0x") {
		base = NumericConstantBase::Hexadecimal;
		//if num_bits.is_none() {
		//	num_bits = Some(4 * (digits_end - 2) as u32);
		//	if is_signed.is_none() {
		//		is_signed = Some(false);
		//	}
		//}
		parse_pure_hex(&s[2..digits_end]).map_err(|e| NumberParseError {
			kind: e.kind,
			range: (0, token_len),
		})?
	}
	else if s.starts_with("0b") {
		base = NumericConstantBase::Binary;
		match is_signed {
			Some(true) => {
				if s.starts_with("0b1"){
					debug!("s: {:?}", &s[2..digits_end]);
					let mut val = parse_pure_binary(parse_two_complement(&s[3..digits_end])?.as_str()).map_err(|e| NumberParseError {
						kind: e.kind,
						range: (0, token_len)})?;
					debug!("val: {:?}", val);
					val += 1;
					debug!("val: {:?}", val);
					val *= -1;
					debug!("val: {:?}", val);
					val
				} else {
					parse_pure_binary(&s[3..digits_end]).map_err(|e| NumberParseError {
						kind: e.kind,
						range: (0, token_len)})?
					}
			},
			Some(false) => parse_pure_binary(&s[2..digits_end]).map_err(|e| NumberParseError {
			kind: e.kind,
			range: (0, token_len),
		})?,
			None => parse_pure_binary(&s[2..digits_end]).map_err(|e| NumberParseError {
			kind: e.kind,
			range: (0, token_len),
		})?,
		}
		//parse_pure_binary(&s[2..digits_end]).map_err(|e| NumberParseError {
		//	kind: e.kind,
		//	range: (0, token_len),
		//})?
	}
	else {
		base = NumericConstantBase::Decimal;
		parse_pure_decimal(&s[0..digits_end])?
	};
	debug!("value: {:?}", value);
	// Create the constant and check if it's valid
	let constant = NumericConstant::new(value, num_bits, is_signed, Some(base));

	// Check if the number can be represented with this number of bits
	if matches!(constant.is_representable(), Some(false)) {
		return Err(NumberParseError {
			kind: NumericConstantParseErrorKind::InsufficientWidth,
			range: (0, token_len),
		});
	}

	Ok(constant)
}

#[cfg(test)]
mod tests {
	use super::*;

	fn check_parse(s: &str, value: u64, num_bits: Option<u32>, is_signed: Option<bool>) {
		let number = parse_numeric_constant_str(s).unwrap();
		assert_eq!(number.value, BigInt::from(value));
		assert_eq!(number.width, num_bits);
		assert_eq!(number.signed, is_signed);
	}

	fn expect_parse_error(s: &str) {
		assert!(parse_numeric_constant_str(s).is_err());
	}

	#[test]
	fn test_parse_plain_numbers() {
		check_parse("64", 64, None, None);
		check_parse("0011", 11, None, None);
		check_parse("000___00__000", 0, None, None);
		check_parse("0", 0, None, None);
		check_parse("0b1101____", 13, None, None);
		check_parse("0x_f_f_", 255, None, None);
	}

	#[test]
	fn test_parse_signedness() {
		check_parse("0s", 0, None, Some(true));
		check_parse("0u", 0, None, Some(false));
		check_parse("123____u", 123, None, Some(false));
		check_parse("1______s____", 1, None, Some(true));
		check_parse("101U", 101, None, Some(false));
		check_parse("0xffS", 255, None, Some(true));
	}

	#[test]
	fn test_parse_fully_constrained() {
		check_parse("0s1", 0, Some(1), Some(true));
		check_parse("1_______s2", 1, Some(2), Some(true));
		check_parse("0x15u15", 21, Some(15), Some(false));
		check_parse("0xFFs00011", 255, Some(11), Some(true));
		check_parse("0b0_1_0_1_u_00__010___", 5, Some(10), Some(false));
		check_parse("0b0_1_0_1_u_00__010___", 5, Some(10), Some(false));

		// I hate this corner case :/
		check_parse("127s8", 127, Some(8), Some(true));
		check_parse("128s8", 128, Some(8), Some(true));
		check_parse("1s1", 1, Some(1), Some(true));
	}

	#[test]
	fn test_insufficient_number_width() {
		expect_parse_error("10u3");
		expect_parse_error("5s3");
	}

	#[test]
	fn test_bad_binary_digits() {
		expect_parse_error("0b4a");
		expect_parse_error("0b1101I");
		expect_parse_error("0b11__2");
	}

	#[test]
	fn test_bad_decimal_digits() {
		expect_parse_error("19a");
		expect_parse_error("19F");
		expect_parse_error("164u124u");
		expect_parse_error("1u2u3u4u5u6u7u64");
		expect_parse_error("144su64");
	}

	#[test]
	fn test_bad_hex_digits() {
		expect_parse_error("0xBROKEN");
		expect_parse_error("0bfa1A0O");
	}

	// TODO add tests for loooong numbers
}
