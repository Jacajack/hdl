pub mod ast;
pub mod grammar_parser;
pub mod parser_context;
pub use grammar_parser::grammar::*;
pub use parser_context::ParserContext;
#[cfg(test)]
mod tests {
	use super::*;
	use crate::parser::ParserContext;
	use crate::lexer::{Lexer, LogosLexer};
	fn parse_expr(s: &str) -> Box<ast::Expression> {
		let lexer = LogosLexer::new(s);
		let mut ctx = ParserContext::new();
		ExprParser::new().parse(&mut ctx, lexer).expect("parsing failed")
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
