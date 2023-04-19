pub mod ast;
pub mod grammar_parser;
pub use grammar_parser::grammar::*;

#[cfg(test)]
mod tests {
	use super::*;
	use super::ast::*;
	use crate::lexer::{Lexer, LogosLexer};

	fn parse_expr(s: &str) -> Box<ast::Expression> {
		let lexer = LogosLexer::new(s);
		ExprParser::new().parse(lexer).expect("parsing failed")
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
		// These tests depend on this unstable feature: https://github.com/rust-lang/rust/issues/29641
		// and will likely need reworking in the future :(
		use Expression::*;

		// First operator - associativity
		assert!(matches!(
			*parse_expr(format!("1 {} 2 {} 3", first, first).as_str()),
			Expression::BinaryExpression {
				lhs: box BinaryExpression{..},
				rhs: box Number{..},
				..
			}
		));

		// Second operator - associativity
		assert!(matches!(
			*parse_expr(format!("1 {} 2 {} 3", second, second).as_str()),
			Expression::BinaryExpression {
				lhs: box BinaryExpression{..},
				rhs: box Number{..},
				..
			}
		));

		// // First before second - natural order
		assert!(matches!(
			*parse_expr(format!("1 {} 2 {} 3", first, second).as_str()),
			Expression::BinaryExpression {
				lhs: box BinaryExpression{..},
				rhs: box Number{..},
				..
			}
		));

		// // First before secone - reverse order
		assert!(matches!(
			*parse_expr(format!("1 {} 2 {} 3", second, first).as_str()),
			Expression::BinaryExpression {
				lhs: box Number{..},
				rhs: box BinaryExpression{..},
				..
			}
		));

		// // Three operators - start from left
		assert!(matches!(
			*parse_expr(format!("1 {} 2 {} 3 {} 4", first, second, second).as_str()),
			BinaryExpression {
				lhs: box BinaryExpression{
					lhs: box BinaryExpression{
						lhs: box Number{..},
						rhs: box Number{..},
						..
					},
					rhs: box Number{..},
					..
				},
				rhs: box Number{..},
				..
			}
		));

		// // Three operators - start from the middle
		assert!(matches!(
			*parse_expr(format!("1 {} 2 {} 3 {} 4", second, first, second).as_str()),
			BinaryExpression {
				lhs: box BinaryExpression{
					lhs: box Number{..},
					rhs: box BinaryExpression{
						lhs: box Number{..},
						rhs: box Number{..},
						..
					},
					..
				},
				rhs: box Number{..},
				..
			}
		));

		// // Three operators - start from the end
		assert!(matches!(
			*parse_expr(format!("1 {} 2 {} 3 {} 4", second, second, first).as_str()),
			BinaryExpression {
				lhs: box BinaryExpression{
					lhs: box Number{..},
					rhs: box Number{..},
					..
				},
				rhs: box BinaryExpression{
					lhs: box Number{..},
					rhs: box Number{..},
					..
				},
				..
			}
		));
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
		use Expression::*;

		assert!(matches!(
			*parse_expr("1 ? 2 : 3 ? 4 : 5"),
			TernaryExpression {
				condition: box Number{..},
				true_branch: box Number{..},
				false_branch: box TernaryExpression{
					condition: box Number{..},
					true_branch: box Number{..},
					false_branch: box Number{..},
					..
				},
				..
			}
		));

		assert!(matches!(
			*parse_expr("1 ? 2 ? 3 : 4 : 5"),
			TernaryExpression {
				condition: box Number{..},
				true_branch: box TernaryExpression{
					condition: box Number{..},
					true_branch: box Number{..},
					false_branch: box Number{..},
					..
				},
				false_branch: box Number{..},
				..
			}
		));
	}
}
