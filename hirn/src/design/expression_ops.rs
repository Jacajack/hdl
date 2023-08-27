use std::ops;
use super::{Expression, BinaryOp, UnaryOp};
use super::expression::{UnaryExpression, BinaryExpression};

macro_rules! impl_binary_op_for_expression {
	($trait: ident, $trait_func: ident, $hirn_op: ident) => {
		impl ops::$trait<Expression> for Expression {
			type Output = Expression;

			fn $trait_func(self, rhs: Expression) -> Self::Output {
				Expression::Binary(BinaryExpression{
					op: BinaryOp::$hirn_op,
					lhs: Box::new(self),
					rhs: Box::new(rhs),
				})
			}
		}
	}
}

macro_rules! impl_unary_op_for_expression {
	($trait: ident, $trait_func: ident, $hirn_op: ident) => {
		impl ops::$trait for Expression {
			type Output = Expression;

			fn $trait_func(self) -> Self::Output {
				Expression::Unary(UnaryExpression{
					op: UnaryOp::$hirn_op,
					operand: Box::new(self),
				})
			}
		}
	}
}

impl_binary_op_for_expression!(Add, add, Add);
impl_binary_op_for_expression!(Sub, sub, Subtract);
impl_binary_op_for_expression!(Div, div, Divide);
impl_binary_op_for_expression!(Mul, mul, Multiply);
impl_binary_op_for_expression!(Rem, rem, Modulo);
impl_binary_op_for_expression!(Shl, shl, ShiftLeft);
impl_binary_op_for_expression!(Shr, shr, ShiftRight);
impl_binary_op_for_expression!(BitAnd, bitand, BitwiseAnd);
impl_binary_op_for_expression!(BitOr, bitor, BitwiseOr);
impl_binary_op_for_expression!(BitXor, bitxor, BitwiseXor);

impl_unary_op_for_expression!(Not, not, LogicalNot);
impl_unary_op_for_expression!(Neg, neg, Negate);

