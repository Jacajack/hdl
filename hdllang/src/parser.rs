pub mod ast;
pub mod grammar_parser;
pub mod parser_context;
pub mod pretty_printer;
pub use grammar_parser::grammar::*;
pub use parser_context::ParserContext;
pub use pretty_printer::PrettyPrinterContext;
extern crate itertools;
use crate::core::compiler_diagnostic::*;
use crate::core::CompilerError;
use crate::lexer::TokenKind;
use crate::SourceSpan;
use lalrpop_util::ParseError;
use std::collections::HashSet;
use std::fmt;
use thiserror::Error;
fn map_token_to_help_msg(expected: &Vec<String>) -> String {
	let mut messages = HashSet::new();
	for token in expected.iter() {
		messages.insert(match token.as_str() {
			"\"MC\"" => "metadata comment",
			"\"async\"" | "\"auto\"" | "\"bool\"" | "\"bus\"" | "\"clock\"" | "\"comb\"" | "\"const\""
			| "\"input\"" | "\"int\"" | "\"output\"" | "\"signed\"" | "\"sync\"" | "\"tristate\"" | "\"unsigned\""
			| "\"wire\"" => "variable block/declaration",
			"\",\"" => "comma",
			"\";\"" => "semicolon",
			"\"(\"" => "left parenthesis",
			"\")\"" => "right parenthesis",
			_ => &token[1..token.len() - 1],
		});
	}
	format!(
		"We expected these productions instead:\n{}",
		itertools::join(messages, ", ")
	)
}
#[derive(Error, Debug)]
pub enum ParserErrorKind {
	#[error("Unexpected token")]
	UnexpectedToken { token: TokenKind, expected: Vec<String> },
	#[error("Invalid token")]
	InvalidToken,
	#[error("Unexpected end of file")]
	UnrecognizedEof { expected: Vec<String> },
	#[error("Lexer error")]
	LexerError(Box<CompilerError>),
}

#[derive(Error, Debug)]
pub struct ParserError {
	kind: ParserErrorKind,
	range: SourceSpan,
}

impl fmt::Display for ParserError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.kind)
	}
}
impl ParserError {
	pub fn new_form_lalrpop_error(e: ParseError<usize, TokenKind, CompilerError>) -> Self {
		use ParserErrorKind::*;
		match e {
			ParseError::InvalidToken { location } => Self {
				kind: InvalidToken,
				range: SourceSpan::new_between(location, location + 1),
			},
			ParseError::UnrecognizedEof { location, expected } => Self {
				kind: UnrecognizedEof { expected },
				range: SourceSpan::new_between(location, location + 1),
			},
			ParseError::UnrecognizedToken { token, expected } => Self {
				kind: UnexpectedToken {
					token: token.1,
					expected,
				},
				range: SourceSpan::new_between(token.0, token.2),
			},
			ParseError::ExtraToken { token } => Self {
				kind: UnexpectedToken {
					token: token.1,
					expected: vec![],
				},
				range: SourceSpan::new_between(token.0, token.2),
			},
			ParseError::User { error } => Self {
				kind: LexerError(Box::new(error)),
				range: SourceSpan::new_between(0, 0),
			},
		}
	}
}
impl ProvidesCompilerDiagnostic for ParserError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		use ParserErrorKind::*;
		match &self.kind {
			UnexpectedToken { token, expected } => CompilerDiagnosticBuilder::from_error(&self)
				.label(self.range, format!("Unexpected token: {:?}", token).as_str())
				.help(&map_token_to_help_msg(expected))
				.build(),
			InvalidToken => CompilerDiagnosticBuilder::from_error(&self)
				.label(self.range, "Invalid token")
				.help("Please replace this token with a valid one.")
				.build(),
			UnrecognizedEof { expected } => CompilerDiagnosticBuilder::from_error(&self)
				.label(self.range, "Unexpected end of file")
				.help(&map_token_to_help_msg(expected))
				.build(),
			LexerError(err) => err.to_diagnostic(),
		}
	}
}

#[cfg(test)]
mod tests {

	use super::*;
	use crate::core::DiagnosticBuffer;
	use crate::lexer::{Lexer, LogosLexer};
	use crate::parser::ParserContext;
	fn parse_expr(s: &str) -> Box<ast::Expression> {
		let lexer = LogosLexer::new(s);
		let buf = Box::new(DiagnosticBuffer::new());
		let mut ctx = ParserContext { diagnostic_buffer: buf };
		ExprParser::new()
			.parse(&mut ctx, Some(&String::from(s)), lexer)
			.expect("parsing failed")
	}

	/// Returns the same expression but with parentheses
	/// This isn't particularly safe or clever but should be
	/// good enough for now.
	fn expr_to_str(s: &str) -> String {
		format!("{:?}", parse_expr(s))
	}

	/// Checks whether the two given opertor have expected precedence
	/// and if both are left-to-right associative
	fn check_precedence(first: &str, second: &str) {
		// First operator - associativity
		assert_eq!(
			expr_to_str(format!("a {} a {} a", first, first).as_str()),
			format!("((foo {} foo) {} foo)", first, first)
		);

		// Second operator - associativity
		assert_eq!(
			expr_to_str(format!("a {} a {} a", second, second).as_str()),
			format!("((foo {} foo) {} foo)", second, second)
		);

		// First before second - natural order
		assert_eq!(
			expr_to_str(format!("a {} a {} a", first, second).as_str()),
			format!("((foo {} foo) {} foo)", first, second)
		);

		// First before secone - reverse order
		assert_eq!(
			expr_to_str(format!("a {} a {} a", second, first).as_str()),
			format!("(foo {} (foo {} foo))", second, first)
		);

		// Three operators - start from left
		assert_eq!(
			expr_to_str(format!("a {} a {} a {} a", first, second, second).as_str()),
			format!("(((foo {} foo) {} foo) {} foo)", first, second, second)
		);

		// Three operators - start from the middle
		assert_eq!(
			expr_to_str(format!("a {} a {} a {} a", second, first, second).as_str()),
			format!("((foo {} (foo {} foo)) {} foo)", second, first, second)
		);

		// Three operators - start from the end
		assert_eq!(
			expr_to_str(format!("a {} a {} a {} a", second, second, first).as_str()),
			format!("((foo {} foo) {} (foo {} foo))", second, second, first)
		);
	}

	#[test]
	fn test_multiplicative_vs_additive() {
		check_precedence("*", "+");
		check_precedence("*", "-");
		check_precedence("/", "+");
		check_precedence("/", "-");
	}

	#[test]
	fn test_additive_vs_shift() {
		check_precedence("+", "<<");
		check_precedence("+", ">>");
		check_precedence("-", "<<");
		check_precedence("-", ">>");
	}

	#[test]
	fn test_shift_vs_bitwise_and() {
		check_precedence("<<", "&");
		check_precedence(">>", "&");
	}

	#[test]
	fn test_bitwise_and_vs_bitwise_xor() {
		check_precedence("&", "^");
		check_precedence("&", "^");
	}

	#[test]
	fn test_bitwise_xor_vs_bitwise_or() {
		check_precedence("^", "|");
		check_precedence("^", "|");
	}

	#[test]
	fn test_bitwise_or_vs_relational() {
		check_precedence("|", ">");
		check_precedence("|", "<");
		check_precedence("|", "<=");
		check_precedence("|", ">=");
	}

	#[test]
	fn test_relational_vs_equality() {
		check_precedence("<", "==");
		check_precedence(">", "==");
		check_precedence("<=", "==");
		check_precedence(">=", "==");
		check_precedence("<", "!=");
		check_precedence(">", "!=");
		check_precedence("<=", "!=");
		check_precedence(">=", "!=");
	}

	#[test]
	fn test_equality_vs_logical_and() {
		check_precedence("==", "&&");
		check_precedence("!=", "&&");
	}

	#[test]
	fn test_logical_and_vs_logical_or() {
		check_precedence("&&", "||");
	}

	#[test]
	fn test_ternary_chaining() {
		assert_eq!(expr_to_str("a ? a : a ? a : a"), "(foo ? foo : (foo ? foo : foo))");
		assert_eq!(expr_to_str("a ? a ? a : a : a"), "(foo ? (foo ? foo : foo) : foo)");
	}
}
