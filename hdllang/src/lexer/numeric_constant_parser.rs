use crate::{compiler_diagnostic::*, core::numeric_constant::*};
//use log::debug;
use num_bigint::BigInt;
use num_traits::Pow;
use std::fmt;
use thiserror::Error;

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
	BigInt::parse_bytes(s.as_bytes(), 10).map_or_else(
		|| {
			Err(NumberParseError {
				kind: NumericConstantParseErrorKind::BadBinaryDigit,
				range: (0, s.len()),
			})
		},
		|f| {
			//debug!("dec: {:?}, bigint: {:?}", s, f);
			Ok(f)
		},
	)
}

/// Parses a pure hex number
fn parse_pure_hex(s: &str) -> Result<BigInt, NumberParseError> {
	BigInt::parse_bytes(s.as_bytes(), 16).map_or_else(
		|| {
			Err(NumberParseError {
				kind: NumericConstantParseErrorKind::BadHexDigit,
				range: (0, s.len()),
			})
		},
		|f| {
			//debug!("hex: 0x{:?}, bigint: {:?}", s, f);
			Ok(f)
		},
	)
}

/// Parses a pure binary number
fn parse_pure_binary(s: &str) -> Result<BigInt, NumberParseError> {
	BigInt::parse_bytes(s.as_bytes(), 2).map_or_else(
		|| {
			Err(NumberParseError {
				kind: NumericConstantParseErrorKind::BadBinaryDigit,
				range: (0, s.len()),
			})
		},
		|f| {
			//debug!("bin: 0b{:?}, bigint: {:?}", s, f);
			Ok(f)
		},
	)
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

			//// Is the number of bits reasonable? // FIXME TO BE DELETED
			//if n > 64 {
			//	return Err(NumberParseError {
			//		kind: NumericConstantParseErrorKind::TooManyBits,
			//		range: (0, token_len),
			//	});
			//}

			if n == 0 {
				return Err(NumberParseError {
					kind: NumericConstantParseErrorKind::InsufficientWidth,
					range: (index + 1, token_len),
				})?;
			}

			num_bits = Some(n);
		}
	}

	// Parse according to base
	let base;
	let value = if s.starts_with("0x") {
		base = NumericConstantBase::Hexadecimal;

		// If width is not specified, assume 4 bits per digit
		if num_bits.is_none() {
			num_bits = Some(4 * (digits_end - 2) as u32);
			if is_signed.is_none() {
				is_signed = Some(false);
			}
		}

		// If the number is signed, we need to check the first digit
		if Some(true) == is_signed {
			let (_, f) = s.char_indices().nth(2).unwrap();
			if f.to_digit(16).unwrap() > 7 {
				let mut d = BigInt::from(f.to_digit(16).unwrap());
				d -= 8;
				d = d * BigInt::from(16).pow((digits_end - 3) as u32);
				let a = BigInt::from(2).pow((digits_end - 2) as u32 * 4 - 1);
				let b = parse_pure_hex(&s[3..digits_end]).map_err(|e| NumberParseError {
					kind: e.kind,
					range: (0, token_len),
				})?;
				b - a + d
			}
			else {
				parse_pure_hex(&s[2..digits_end]).map_err(|e| NumberParseError {
					kind: e.kind,
					range: (0, token_len),
				})?
			}
		}
		else {
			parse_pure_hex(&s[2..digits_end]).map_err(|e| NumberParseError {
				kind: e.kind,
				range: (0, token_len),
			})?
		}
	}
	else if s.starts_with("0b") {
		base = NumericConstantBase::Binary;
		num_bits = num_bits.or(Some((digits_end - 2) as u32));
		is_signed = is_signed.or(Some(false));

		let mut val = parse_pure_binary(&s[2..digits_end]).map_err(|e| NumberParseError {
			kind: e.kind,
			range: (0, token_len),
		})?;

		if is_signed.unwrap() && val != 0.into() && val.bit(num_bits.unwrap() as u64 - 1) {
			val -= BigInt::from(1) << num_bits.unwrap();
		}

		val
	}
	else {
		base = NumericConstantBase::Decimal;
		//if is_signed.is_none() {
		//	is_signed = Some(true);
		//}
		parse_pure_decimal(&s[0..digits_end])?
	};
	//debug!("Succesfully parsed value: {:?}", value);
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
	use rstest::rstest;

	fn check_parse(s: &str, value: i64, num_bits: Option<u32>, is_signed: Option<bool>) {
		let number = parse_numeric_constant_str(s).unwrap();
		assert_eq!(number.value, BigInt::from(value));
		assert_eq!(number.width, num_bits);
		assert_eq!(number.signed, is_signed);
	}

	fn expect_parse_error(s: &str) {
		assert!(parse_numeric_constant_str(s).is_err());
	}

	#[rstest]
	#[case("64", 64, None, None)]
	#[case("0011", 11, None, None)]
	#[case("000___00__000", 0, None, None)]
	#[case("0", 0, None, None)]
	#[case("0b1101____", 13, Some(4), Some(false))]
	#[case("0b1101____s", 5-8, Some(4), Some(true))]
	#[case("0b1101____s128", 13, Some(128), Some(true))]
	#[case("0b1101____u128", 13, Some(128), Some(false))]
	#[case("0x_f_f_", 255, Some(8), Some(false))]
	#[case("0s", 0, None, Some(true))]
	#[case("0u", 0, None, Some(false))]
	#[case("123____u", 123, None, Some(false))]
	#[case("1______s____", 1, None, Some(true))]
	#[case("101U", 101, None, Some(false))]
	#[case("0xffS", -1, Some(8), Some(true))]
	#[case("0s1", 0, Some(1), Some(true))]
	#[case("1_______s2", 1, Some(2), Some(true))]
	#[case("0x15u15", 21, Some(15), Some(false))]
	#[case("0xFFs00011", -1, Some(11), Some(true))]
	#[case("0b0_1_0_1_u_00__010___", 5, Some(10), Some(false))]
	#[case("0b0_1_0_1_u_00__010___", 5, Some(10), Some(false))]
	#[case("127s8", 127, Some(8), Some(true))]
	#[case("128s8", 128, Some(8), Some(true))]
	#[case("1s1", 1, Some(1), Some(true))]
	fn test_valid_numbers(
		#[case] s: &str,
		#[case] value: i64,
		#[case] num_bits: Option<u32>,
		#[case] is_signed: Option<bool>,
	) {
		check_parse(s, value, num_bits, is_signed);
	}

	#[rstest]
	#[case("16u4")]
	#[case("8u3")]
	#[case("0u0")]
	#[case("1u0")]
	#[case("0s0")]
	#[case("1s0")]
	#[case("0000s000000")]
	#[case("100_u_0_____0")]
	#[case("0xfu3")]
	fn test_invalid_numbers(#[case] s: &str) {
		expect_parse_error(s);
	}

	#[rstest]
	#[case("10u3")]
	#[case("5s3")]
	fn test_insufficient_number_width(#[case] s: &str) {
		expect_parse_error(s);
	}

	#[rstest]
	#[case("0b2")]
	#[case("0b4a")]
	#[case("0b1101I")]
	#[case("0b11__2")]
	fn test_bad_binary_digits(#[case] s: &str) {
		expect_parse_error(s);
	}

	#[rstest]
	#[case("19a")]
	#[case("19F")]
	#[case("164u124u")]
	#[case("1u2u3u4u5u6u7u64")]
	#[case("144su64")]
	fn test_bad_decimal_digits(#[case] s: &str) {
		expect_parse_error(s);
	}

	#[rstest]
	#[case("0xBROKEN")]
	#[case("0bfa1A0O")]
	fn test_bad_hex_digits(#[case] s: &str) {
		expect_parse_error(s);
	}

	// TODO add tests for loooong numbers
}
