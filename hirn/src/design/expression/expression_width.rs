use super::{Expression, UnaryExpression, BinaryExpression, BuiltinOp, ConditionalExpression, UnaryOp, BinaryOp};

pub trait WidthExpression {
	fn width(&self) -> Expression;
}

impl WidthExpression for UnaryExpression {
	fn width(&self) -> Expression {
		use UnaryOp::*;
		match self.op {
			Negate => self.operand.width(),
			LogicalNot => 1.into(),
			BitwiseNot => self.operand.width(),
			ReductionAnd | ReductionOr | ReductionXor => 1.into(),
		}
	}
}

impl WidthExpression for BinaryExpression {
	fn width(&self) -> Expression {
		use BinaryOp::*;
		match self.op {
			Add | Subtract => self.lhs.width().max(self.rhs.width()) + 1.into(),
			Multiply => self.lhs.width() + self.rhs.width(),
			Divide => self.lhs.width(), // FIXME
			Modulo => self.lhs.width(), // FIXME
			ShiftLeft | ShiftRight => self.lhs.width(),
			LogicalAnd | LogicalOr => 1.into(),
			BitwiseAnd | BitwiseOr | BitwiseXor => self.lhs.width(), // FIXME verify width match
			Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual => 1.into(),
			Max | Min => self.lhs.width(),
		}
	}
}

impl WidthExpression for BuiltinOp {
	fn width(&self) -> Expression {
		use BuiltinOp::*;
		match self {
			ZeroExtend { width, ..} => (**width).clone(),
			SignExtend { width, ..} => (**width).clone(),
			BusSelect { msb, lsb, ..} => (**msb).clone() - (**lsb).clone() + 1.into(),
			BitSelect {..} => 1.into(),
			Replicate { expr, count } => (**expr).clone() * (**count).clone(),
			Width(..) => 64.into(), // FIXME?
			Join(exprs) => {
				let mut result = Expression::new_zero();
				for ex in exprs {
					result = result + ex.width();
				}
				result
			}
		}
	}
}

impl WidthExpression for ConditionalExpression {
	fn width(&self) -> Expression {
		self.default_value().width()
	}
}

impl WidthExpression for Expression {
	fn width(&self) -> Expression {
		use Expression::*;

		match self {
			Binary(expr) => expr.width(),
			Unary(expr) => expr.width(),
			Constant(constant) => constant.width().into(),
			Signal(slice) => BuiltinOp::Width(Box::new((*slice).clone().into())).into(),
			Conditional(expr) => expr.width(),
			Builtin(builtin) => builtin.width(),
			Cast(expr) => expr.src.width(),
		}
	}
}
