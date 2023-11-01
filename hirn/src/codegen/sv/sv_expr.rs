use std::collections::HashMap;

use super::SVCodegen;
use crate::{
	codegen::CodegenError,
	design::{
		BinaryExpression, BinaryOp, BuiltinOp, CastExpression, ConditionalExpression, DesignHandle, EvalContext,
		Evaluates, EvaluatesType, Expression, HasSensitivity, HasSignedness, NumericConstant, SignalClass, SignalId,
		SignalSensitivity, SignalSignedness, SignalSlice, UnaryExpression, UnaryOp, WidthExpression,
	},
};

pub(super) struct IntermediateSignal {
	name: String,
	width: Expression,
	signedness: SignalSignedness,
	sensitivity: SignalSensitivity,
	value: Expression,
}

impl IntermediateSignal {
	pub(super) fn class(&self) -> SignalClass {
		SignalClass::new(self.width.clone(), self.signedness())
	}

	pub(super) fn name(&self) -> &str {
		&self.name
	}

	pub(super) fn value(&self) -> &Expression {
		&self.value
	}
}

impl HasSensitivity for IntermediateSignal {
	fn sensitivity(&self) -> &SignalSensitivity {
		&self.sensitivity
	}
}

impl HasSignedness for IntermediateSignal {
	fn signedness(&self) -> SignalSignedness {
		self.signedness
	}
}

pub(super) struct SVExpressionCodegen {
	design: DesignHandle,
	tmp_counter: u32,
	width_casts: Vec<bool>,
	intermediates: Vec<IntermediateSignal>,
	signal_substitutions: Option<HashMap<SignalId, String>>, // FIXME we don't want to copy that each time
}

impl SVExpressionCodegen {
	pub fn new(
		design: DesignHandle,
		width_casts: bool,
		tmp_counter_start: u32,
		signal_subs: Option<HashMap<SignalId, String>>,
	) -> Self {
		Self {
			design,
			tmp_counter: tmp_counter_start,
			width_casts: vec![width_casts],
			intermediates: Vec::new(),
			signal_substitutions: signal_subs,
		}
	}

	pub fn intermediates(&self) -> &[IntermediateSignal] {
		&self.intermediates
	}

	pub fn get_tmp_counter(&self) -> u32 {
		self.tmp_counter
	}

	fn width_casts(&self) -> bool {
		*self.width_casts.last().unwrap()
	}

	fn push_width_casts(&mut self, width_casts: bool) {
		self.width_casts.push(width_casts);
	}

	fn pop_width_casts(&mut self) {
		self.width_casts.pop();
	}

	fn translate_signal_id(&self, id: SignalId) -> String {
		if let Some(subs) = &self.signal_substitutions {
			if let Some(sub) = subs.get(&id) {
				return sub.clone();
			}
		}

		let sig = self.design.get_signal(id).unwrap();
		sig.name().into()
	}

	fn translate_conditional_expression(&mut self, expr: &ConditionalExpression) -> Result<String, CodegenError> {
		if expr.branches().len() == 0 {
			return Ok(self.translate_expression_no_preprocess(expr.default_value())?);
		}

		let mut str: String = "".into();
		for br in expr.branches() {
			str = format!(
				"{} ({}) ? ({}) : ",
				str,
				self.translate_expression_no_preprocess(&br.condition())?,
				self.translate_expression_no_preprocess(&br.value())?
			);
		}
		Ok(format!(
			"{} ({})",
			str,
			self.translate_expression_no_preprocess(expr.default_value())?
		))
	}

	fn translate_constant(&self, c: &NumericConstant) -> Result<String, CodegenError> {
		if self.width_casts() {
			let is_signed = c.signedness().unwrap_or(SignalSignedness::Unsigned).is_signed();
			Ok(format!(
				"{}'{}h{}",
				c.width()?,
				if is_signed { "s" } else { "" },
				c.to_hex_str()?
			))
		}
		else {
			Ok(format!("{}", c.to_dec_str()?))
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

	fn translate_binary_expression(&mut self, expr: &BinaryExpression) -> Result<String, CodegenError> {
		let lhs_str = self.translate_expression_no_preprocess(&expr.lhs)?;
		let rhs_str = self.translate_expression_no_preprocess(&expr.rhs)?;

		// Special handling for min/max operators
		use BinaryOp::*;
		match expr.op {
			Min => return self.translate_min_max_expression(false, &lhs_str, &rhs_str),
			Max => return self.translate_min_max_expression(true, &lhs_str, &rhs_str),
			_ => (),
		}

		let casts_needed = match expr.op {
			// Relational operators work on operands of the same width
			Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual => false,

			// Bitwise operators work on operands of the same width
			BitwiseAnd | BitwiseOr | BitwiseXor => false,

			// Logical operators only work on booleans (same width)
			LogicalAnd | LogicalOr => false,

			// Arithmetic operators allow mixed width
			Add | Subtract | Multiply | Divide | Modulo => true,

			// Shift operators allow mixed width
			ShiftLeft | ShiftRight => true,

			Min | Max => unreachable!("Min and Max should be handled separately"),
		};

		let cast_str = if casts_needed && self.width_casts() {
			self.push_width_casts(false);
			let str = self.translate_expression_try_eval(&expr.width()?)?;
			self.pop_width_casts();
			Some(str)
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
			Equal => "==",
			NotEqual => "!=",
			Less => "<",
			LessEqual => "<=",
			Greater => ">",
			GreaterEqual => ">=",
			LogicalAnd => "&&",
			LogicalOr => "||",
			Max | Min => unreachable!("Max and Min should be handled separately"),
			ShiftLeft | ShiftRight => {
				let eval_ctx = EvalContext::without_assumptions(self.design.clone());
				let lhs_type = expr.lhs.eval_type(&eval_ctx)?;
				let arith_shift = lhs_type.is_signed();
				match (arith_shift, expr.op) {
					(false, ShiftLeft) => "<<",
					(true, ShiftLeft) => "<<<",
					(false, ShiftRight) => ">>",
					(true, ShiftRight) => ">>>",
					_ => unreachable!("Other operations are handled by the outer match"),
				}
			},
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

	fn translate_unary(&mut self, expr: &UnaryExpression) -> Result<String, CodegenError> {
		let operand_str = self.translate_expression_no_preprocess(&expr.operand)?;
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

	fn translate_signal_slice(&mut self, slice: &SignalSlice) -> Result<String, CodegenError> {
		let mut str: String = self.translate_signal_id(slice.signal);
		for index_expr in &slice.indices {
			str = format!("{}[{}]", str, self.translate_expression_no_preprocess(&index_expr)?);
		}
		Ok(str)
	}

	fn new_intermediate(&mut self, expr: &Expression) -> Result<String, CodegenError> {
		let eval_ctx = EvalContext::without_assumptions(self.design.clone());
		let lhs_type = expr.eval_type(&eval_ctx)?;
		let name = format!("hirn_tmp_{}$", self.tmp_counter);
		self.intermediates.push(IntermediateSignal {
			name: name.clone(),
			width: expr.width()?,
			signedness: lhs_type.signedness(),
			sensitivity: lhs_type.sensitivity().clone(),
			value: expr.clone(),
		});
		self.tmp_counter += 1;
		Ok(name)
	}

	fn translate_suffix_op_lhs(&mut self, lhs: &Expression) -> Result<String, CodegenError> {
		match lhs {
			Expression::Signal(slice) => self.translate_signal_slice(slice),
			lhs => self.new_intermediate(lhs),
		}
	}

	fn translate_builtin_op(&mut self, op: &BuiltinOp) -> Result<String, CodegenError> {
		use BuiltinOp::*;
		Ok(match op {
			Width(_) => unimplemented!("$bits() shall never be emitted"),
			ZeroExtend { .. } | SignExtend { .. } => {
				unimplemented!("Zero/sign extensions shall be expanded before codegen")
			},

			BitSelect { expr, index } => {
				let expr_str = self.translate_suffix_op_lhs(&expr)?;
				self.push_width_casts(false);
				let index_str = self.translate_expression_try_eval(&index)?;
				self.pop_width_casts();
				format!("{}[{}]", expr_str, index_str)
			},

			BusSelect { expr, msb, lsb } => {
				let expr_str = self.translate_suffix_op_lhs(&expr)?;
				self.push_width_casts(false);
				let msb_str = self.translate_expression_try_eval(&msb)?;
				let lsb_str = self.translate_expression_try_eval(&lsb)?;
				self.pop_width_casts();
				format!("{}[{}:{}]", expr_str, msb_str, lsb_str)
			},

			Replicate { expr, count } => {
				let expr_str = self.translate_expression_no_preprocess(&expr)?;
				self.push_width_casts(false);
				let count_str = self.translate_expression_try_eval(&count)?;
				self.pop_width_casts();
				format!("{{ {} {{ {} }} }}", count_str, expr_str)
			},

			Join(exprs) => {
				let mut str = "{ ".into();
				for (index, expr) in exprs.iter().enumerate() {
					str = format!(
						"{}{}{}",
						str,
						self.translate_expression_no_preprocess(&expr)?,
						if index == exprs.len() - 1 { "" } else { ", " }
					);
				}
				format!("{} }}", str)
			},
		})
	}

	fn translate_cast_expression(&mut self, expr: &CastExpression) -> Result<String, CodegenError> {
		let expr_str = self.translate_expression_no_preprocess(&expr.src)?;
		use SignalSignedness::*;
		match expr.signedness {
			Some(Signed) => Ok(format!("$signed({})", expr_str)),
			Some(Unsigned) => Ok(format!("$unsigned({})", expr_str)),
			None => Ok(expr_str),
		}
	}

	fn translate_expression_no_preprocess(&mut self, expr: &Expression) -> Result<String, CodegenError> {
		use Expression::*;
		match expr {
			Conditional(expr) => self.translate_conditional_expression(expr),
			Constant(c) => self.translate_constant(c),
			Signal(slice) => self.translate_signal_slice(slice),
			Binary(expr) => self.translate_binary_expression(expr),
			Unary(expr) => self.translate_unary(expr),
			Builtin(op) => self.translate_builtin_op(op),
			Cast(c) => self.translate_cast_expression(c),
		}
	}

	pub(super) fn translate_expression(&mut self, expr: &Expression) -> Result<String, CodegenError> {
		self.translate_expression_no_preprocess(&self.preprocess_expression(expr.clone())?)
	}

	pub(super) fn translate_expression_try_eval(&mut self, expr: &Expression) -> Result<String, CodegenError> {
		let preprocessed = self.preprocess_expression(expr.clone())?;
		match preprocessed.const_eval() {
			Ok(value) => self.translate_constant(&value),
			Err(_) => self.translate_expression_no_preprocess(&preprocessed),
		}
	}
}
