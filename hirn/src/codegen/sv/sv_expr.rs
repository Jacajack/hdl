use super::SVCodegen;
use crate::{
	codegen::CodegenError,
	design::{
		BinaryExpression, BinaryOp, BuiltinOp, ConditionalExpression, Evaluates, Expression, NumericConstant, SignalId,
		SignalSlice, UnaryExpression, UnaryOp, WidthExpression, SignalSignedness, SignalSensitivity, HasSensitivity, HasSignedness, EvalContext, EvaluatesType, DesignHandle, SignalClass, Design,
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

pub(super) struct SVExpressionCodegen<'a> {
	design: &'a Design,
	tmp_counter: u32,
	width_casts: bool,
	intermediates: Vec<IntermediateSignal>,
}

impl<'a> SVExpressionCodegen<'a> {
	pub fn new(design: &'a Design, width_casts: bool, tmp_counter_start: u32) -> Self {
		Self {
			design,
			tmp_counter: tmp_counter_start,
			width_casts,
			intermediates: Vec::new(),
		}
	}

	pub fn intermediates(&self) -> &[IntermediateSignal] {
		&self.intermediates
	}

	pub fn get_tmp_counter(&self) -> u32 {
		self.tmp_counter
	}

	fn translate_signal_id(&self, id: SignalId) -> String {
		let sig = self.design.get_signal(id).unwrap();
		sig.name().into()
	}

	fn translate_conditional_expression(
		&mut self,
		expr: &ConditionalExpression,
	) -> Result<String, CodegenError> {
		assert!(expr.branches().len() > 0);
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
		if self.width_casts {
			Ok(format!("{}'h{}", c.width()?, c.to_hex_str()?))
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

		let cast_str = if self.width_casts {
			let mut cg = SVExpressionCodegen::new(
				self.design,
				false,
				self.get_tmp_counter());
			let str = cg.translate_expression_try_eval(&expr.width()?)?;
			self.tmp_counter = cg.get_tmp_counter();
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
			ShiftLeft => "<<", // FIXME arithmetic vs logical
			ShiftRight => ">>", // FIXME arithemtic vs logical
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
			str = format!(
				"{}[{}]",
				str,
				self.translate_expression_no_preprocess(&index_expr)?
			);
		}
		Ok(str)
	}

	fn new_intermediate(&mut self, expr: &Expression) -> Result<String, CodegenError> {
		let eval_ctx = EvalContext::without_assumptions(self.design.handle());
		let lhs_type = expr.eval_type(&eval_ctx)?;
		let name = format!("hirn_tmp_{}$", self.tmp_counter);
		self.intermediates.push(IntermediateSignal{
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
			Width(e) => unimplemented!("$bits() shall never be emitted"),
			ZeroExtend { .. } | SignExtend { .. } => {
				unimplemented!("Zero/sign extensions shall be expanded before codegen")
			},

			BitSelect { expr, index } => {
				let expr_str = self.translate_suffix_op_lhs(&expr)?;
				let index_str = self.translate_expression_no_preprocess(&index)?;
				format!("{}[{}]", expr_str, index_str)
			},

			BusSelect { expr, msb, lsb } => {
				let expr_str = self.translate_suffix_op_lhs(&expr)?;
				let msb_str = self.translate_expression_no_preprocess(&msb)?;
				let lsb_str = self.translate_expression_no_preprocess(&lsb)?;
				format!("{}[{}:{}]", expr_str, msb_str, lsb_str)
			},

			Replicate { expr, count } => {
				let expr_str = self.translate_expression_no_preprocess(&expr)?;
				let count_str = self.translate_expression_no_preprocess(&count)?;
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

	fn translate_expression_no_preprocess(&mut self, expr: &Expression) -> Result<String, CodegenError> {
		use Expression::*;
		match expr {
			Conditional(expr) => self.translate_conditional_expression(expr),
			Constant(c) => self.translate_constant(c),
			Signal(slice) => self.translate_signal_slice(slice),
			Binary(expr) => self.translate_binary_expression(expr),
			Unary(expr) => self.translate_unary(expr),
			Builtin(op) => self.translate_builtin_op(op),
			Cast(c) => self.translate_expression_no_preprocess(&c.src),
		}
	}

	pub(super) fn translate_expression(&mut self, expr: &Expression) -> Result<String, CodegenError> {
		self.translate_expression_no_preprocess(&self.preprocess_expression(expr.clone())?)
	}

	pub(super) fn translate_expression_try_eval(
		&mut self,
		expr: &Expression
	) -> Result<String, CodegenError> {
		let preprocessed = self.preprocess_expression(expr.clone())?;
		match preprocessed.const_eval() {
			Ok(value) => self.translate_constant(&value),
			Err(_) => self.translate_expression_no_preprocess(&preprocessed),
		}
	}
}
