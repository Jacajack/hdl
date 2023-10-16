use super::SVCodegen;
use crate::{design::{Expression, SignalId, BuiltinOp, UnaryOp, BinaryOp, NumericConstant, WidthExpression, ConditionalExpression, BinaryExpression, UnaryExpression, SignalSlice}, codegen::CodegenError};

impl<'a> SVCodegen<'a> {
	fn translate_signal_id(&self, id: SignalId) -> String {
		let sig = self.design.get_signal(id).unwrap();
		sig.name().into()
	}

	fn translate_conditional_expression(&self, expr: &ConditionalExpression, width_casts: bool) -> Result<String, CodegenError> {
		assert!(expr.branches().len() > 0);
		let mut str: String = "".into();
		for br in expr.branches() {
			str = format!(
				"{} ({}) ? ({}) : ",
				str,
				self.translate_expression(&br.condition(), width_casts)?,
				self.translate_expression(&br.value(), width_casts)?
			);
		}
		Ok(format!(
			"{} ({})",
			str,
			self.translate_expression(expr.default_value(), width_casts)?
		))
	}

	fn translate_constant(&self, c: &NumericConstant, width_casts: bool) -> Result<String, CodegenError> {
		Ok(format!("{}'h{}", c.width().unwrap(), c.to_hex_str())) // FIXME unwrap!
	}

	fn translate_binary_expression(&self, expr: &BinaryExpression, width_casts: bool) -> Result<String, CodegenError> {
		let lhs_str = self.translate_expression(&expr.lhs, width_casts)?;
		let rhs_str = self.translate_expression(&expr.rhs, width_casts)?;
		// TODO proper width rules
		use BinaryOp::*;
		let str = if width_casts {
			match expr.op {
				Add => {
					let cast_str = self.translate_expression(&expr.width().unwrap(), false)?; // FIXME unwrap!
					format!("(({})'({}) + ({})'({}))", cast_str, lhs_str, cast_str, rhs_str)
				},
				_ => todo!(), // TODO
			}
		}
		else {
			match expr.op {
				Add => format!("({} + {})", lhs_str, rhs_str),
				Subtract => format!("{} - {}", lhs_str, rhs_str),
				Multiply => format!("{} * {}", lhs_str, rhs_str),
				Divide => format!("{} * {}", lhs_str, rhs_str),
				Max => format!("({} > {} ? {} : {})", lhs_str, rhs_str, lhs_str, rhs_str),
				_ => todo!(),
			}
		};

		Ok(str)
	}

	fn translate_unary(&self, expr: &UnaryExpression, width_casts: bool) -> Result<String, CodegenError> {
		let operand_str = self.translate_expression(&expr.operand, width_casts)?;
		use UnaryOp::*;
		Ok(match expr.op {
			Negate => format!("-{}", operand_str),
			LogicalNot => format!("!{}", operand_str),
			BitwiseNot => format!("~{}", operand_str),
			ReductionAnd => format!("&{}", operand_str),
			ReductionOr => format!("|{}", operand_str),
			ReductionXor => format!("^{}", operand_str),
		})
	}

	fn translate_signal_slice(&self, slice: &SignalSlice, width_casts: bool) -> Result<String, CodegenError> {
		let mut str: String = self.translate_signal_id(slice.signal);
		for index_expr in &slice.indices {
			str = format!("{}[{}]", str, self.translate_expression(&index_expr, width_casts)?);
		}
		Ok(str)
	}

	fn translate_builtin_op(&self, op: &BuiltinOp, width_casts: bool) -> Result<String, CodegenError> {
		Ok(match op {
			BuiltinOp::Width(e) => format!("$bits({})", self.translate_expression(&e, width_casts)?),
			_ => todo!(),
		})
	}

	pub(super) fn translate_expression(&self, expr: &Expression, width_casts: bool) -> Result<String, CodegenError> {
		use Expression::*;
		match expr {
			Conditional(expr) => self.translate_conditional_expression(expr, width_casts),
			Constant(c) => self.translate_constant(c, width_casts),
			Signal(slice) => self.translate_signal_slice(slice, width_casts),
			Binary(expr) => self.translate_binary_expression(expr, width_casts),
			Unary(expr) => self.translate_unary(expr, width_casts),
			Builtin(op) => self.translate_builtin_op(op, width_casts),
			Cast(c) => self.translate_expression(&c.src, width_casts),
		}
	}
}