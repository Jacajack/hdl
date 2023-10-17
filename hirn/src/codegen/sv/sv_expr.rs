use super::SVCodegen;
use crate::{
	codegen::CodegenError,
	design::{
		BinaryExpression, BinaryOp, BuiltinOp, ConditionalExpression, Evaluates, Expression, NumericConstant, SignalId,
		SignalSlice, UnaryExpression, UnaryOp, WidthExpression,
	},
};

impl<'a> SVCodegen<'a> {
	fn translate_signal_id(&self, id: SignalId) -> String {
		let sig = self.design.get_signal(id).unwrap();
		sig.name().into()
	}

	fn translate_conditional_expression(
		&self,
		expr: &ConditionalExpression,
		width_casts: bool,
	) -> Result<String, CodegenError> {
		assert!(expr.branches().len() > 0);
		let mut str: String = "".into();
		for br in expr.branches() {
			str = format!(
				"{} ({}) ? ({}) : ",
				str,
				self.translate_expression_no_preprocess(&br.condition(), width_casts)?,
				self.translate_expression_no_preprocess(&br.value(), width_casts)?
			);
		}
		Ok(format!(
			"{} ({})",
			str,
			self.translate_expression_no_preprocess(expr.default_value(), width_casts)?
		))
	}

	fn translate_constant(&self, c: &NumericConstant, width_casts: bool) -> Result<String, CodegenError> {
		if width_casts {
			Ok(format!("{}'h{}", c.width()?, c.to_hex_str()))
		}
		else {
			Ok(format!("'h{}", c.to_hex_str()))
		}
	}

	fn translate_min_max_expression(&self, is_max: bool, lhs_str: &str, rhs_str: &str) -> Result<String, CodegenError> {
		Ok(format!(
			"({} {} {} ? {} : {})",
			lhs_str,
			if is_max { ">" } else { "<" },
			rhs_str,
			lhs_str,
			rhs_str
		))
	}

	fn expression_substitute(&self, e: &mut Expression) -> Result<(), CodegenError> {
		use Expression::*;
		match e {
			Builtin(op) => match op {
				// We're aiming to avoid using System Verilog's $bits() function
				BuiltinOp::Width(width_arg) => match **width_arg {
					// Eradicate trivial width() called on slices
					Signal(ref slice) => {
						// FIXME this likely requires more graceful handling
						let signal = self.design.get_signal(slice.signal).expect("Invalid signal ID");
						*e = signal.class.width().clone().cast_unsigned();
					},

					// Eradicate trivial width() called on constants
					Constant(ref nc) => {
						*e = Constant(nc.width()?.into());
					},

					// Expand width() non-trivial expressions
					_ => {
						*e = e.width()?;
					},
				},

				// Expand zero-extension builtins
				BuiltinOp::ZeroExtend {
					expr: argument,
					width: desired_width,
				} => {
					*e = Expression::new_join(vec![
						Expression::new_replicate(
							NumericConstant::new_bool(false).into(),
							(**desired_width).clone() - argument.width()?,
						),
						(**argument).clone(),
					]);
				},

				// Expand zero-extension builtins
				BuiltinOp::SignExtend {
					expr: argument,
					width: desired_width,
				} => {
					*e = Expression::new_join(vec![
						Expression::new_replicate(
							argument.clone().bit_select(argument.width()? - 1u32.into()), // MSB
							(**desired_width).clone() - argument.width()?,
						),
						(**argument).clone(),
					]);
				},

				_ => {},
			},
			_ => {},
		}

		Ok(())
	}

	fn preprocess_expression(&self, expression: Expression) -> Result<Expression, CodegenError> {
		let mut expr = expression;
		expr.transform(&|e| self.expression_substitute(e))?;
		Ok(expr)
	}

	fn translate_binary_expression(&self, expr: &BinaryExpression, width_casts: bool) -> Result<String, CodegenError> {
		let lhs_str = self.translate_expression_no_preprocess(&expr.lhs, width_casts)?;
		let rhs_str = self.translate_expression_no_preprocess(&expr.rhs, width_casts)?;

		// Special handling for min/max operators
		use BinaryOp::*;
		match expr.op {
			Min => return self.translate_min_max_expression(false, &lhs_str, &rhs_str),
			Max => return self.translate_min_max_expression(true, &lhs_str, &rhs_str),
			_ => (),
		}

		let cast_str = if width_casts {
			Some(self.translate_expression_try_eval(&expr.width()?, false)?)
		}
		else {
			None
		};

		let operator_str = match expr.op {
			Add => "+",
			Subtract => "-",
			Multiply => "*",
			Divide => "/",
			Modulo => "%",
			BitwiseAnd => "&",
			BitwiseOr => "|",
			BitwiseXor => "^",
			ShiftLeft => "<<",
			ShiftRight => ">>",
			Equal => "==",
			NotEqual => "!=",
			Less => "<",
			LessEqual => "<=",
			Greater => ">",
			GreaterEqual => ">=",
			LogicalAnd => "&&",
			LogicalOr => "||",
			Max | Min => unreachable!("Max and Min should be handled separately"),
		};

		let expr_str = match cast_str {
			Some(cast_str) => format!(
				"(({})'({}) {} ({})'({}))",
				cast_str, lhs_str, operator_str, cast_str, rhs_str
			),
			None => format!("({} {} {})", lhs_str, operator_str, rhs_str),
		};

		Ok(expr_str)
	}

	fn translate_unary(&self, expr: &UnaryExpression, width_casts: bool) -> Result<String, CodegenError> {
		let operand_str = self.translate_expression_no_preprocess(&expr.operand, width_casts)?;
		use UnaryOp::*;
		Ok(match expr.op {
			Negate => format!("(-{})", operand_str),
			LogicalNot => format!("(!{})", operand_str),
			BitwiseNot => format!("(~{})", operand_str),
			ReductionAnd => format!("(&{})", operand_str),
			ReductionOr => format!("(|{})", operand_str),
			ReductionXor => format!("(^{})", operand_str),
		})
	}

	fn translate_signal_slice(&self, slice: &SignalSlice, width_casts: bool) -> Result<String, CodegenError> {
		let mut str: String = self.translate_signal_id(slice.signal);
		for index_expr in &slice.indices {
			str = format!(
				"{}[{}]",
				str,
				self.translate_expression_no_preprocess(&index_expr, width_casts)?
			);
		}
		Ok(str)
	}

	fn translate_builtin_op(&self, op: &BuiltinOp, width_casts: bool) -> Result<String, CodegenError> {
		use BuiltinOp::*;
		Ok(match op {
			Width(e) => unimplemented!("$bits() shall never be emitted"),
			ZeroExtend { .. } | SignExtend { .. } => {
				unimplemented!("Zero/sign extensions shall be expanded before codegen")
			},

			BitSelect { expr, index } => {
				let expr_str = self.translate_expression_no_preprocess(&expr, width_casts)?;
				let index_str = self.translate_expression_no_preprocess(&index, width_casts)?;
				format!("{}[{}]", expr_str, index_str)
			},

			BusSelect { expr, msb, lsb } => {
				let expr_str = self.translate_expression_no_preprocess(&expr, width_casts)?;
				let msb_str = self.translate_expression_no_preprocess(&msb, width_casts)?;
				let lsb_str = self.translate_expression_no_preprocess(&lsb, width_casts)?;
				format!("{}[{}:{}]", expr_str, msb_str, lsb_str)
			},

			Replicate { expr, count } => {
				let expr_str = self.translate_expression_no_preprocess(&expr, width_casts)?;
				let count_str = self.translate_expression_no_preprocess(&count, width_casts)?;
				format!("{{ {} {{ {} }} }}", count_str, expr_str)
			},

			Join(exprs) => {
				let mut str = "{ ".into();
				for (index, expr) in exprs.iter().enumerate() {
					str = format!(
						"{}{}{}",
						str,
						self.translate_expression_no_preprocess(&expr, width_casts)?,
						if index == exprs.len() - 1 { "" } else { ", " }
					);
				}
				format!("{} }}", str)
			},
		})
	}

	fn translate_expression_no_preprocess(&self, expr: &Expression, width_casts: bool) -> Result<String, CodegenError> {
		use Expression::*;
		match expr {
			Conditional(expr) => self.translate_conditional_expression(expr, width_casts),
			Constant(c) => self.translate_constant(c, width_casts),
			Signal(slice) => self.translate_signal_slice(slice, width_casts),
			Binary(expr) => self.translate_binary_expression(expr, width_casts),
			Unary(expr) => self.translate_unary(expr, width_casts),
			Builtin(op) => self.translate_builtin_op(op, width_casts),
			Cast(c) => self.translate_expression_no_preprocess(&c.src, width_casts),
		}
	}

	pub(super) fn translate_expression(&self, expr: &Expression, width_casts: bool) -> Result<String, CodegenError> {
		self.translate_expression_no_preprocess(&self.preprocess_expression(expr.clone())?, width_casts)
	}

	pub(super) fn translate_expression_try_eval(
		&self,
		expr: &Expression,
		width_casts: bool,
	) -> Result<String, CodegenError> {
		let preprocessed = self.preprocess_expression(expr.clone())?;
		match preprocessed.const_eval() {
			Ok(value) => self.translate_constant(&value, width_casts),
			Err(_) => self.translate_expression_no_preprocess(&preprocessed, width_casts),
		}
	}
}
