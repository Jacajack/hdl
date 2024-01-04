use std::cmp::max;

use hirn::design::Evaluates;
use num_bigint::BigInt;

use super::Expression;
use crate::{
	analyzer::{
		AdditionalContext, BusWidth, GlobalAnalyzerContext, LocalAnalyzerContext, ModuleImplementationScope,
		SemanticError, SignalSensitivity, SignalSignedness,
	},
	core::NumericConstant,
	parser::ast::{MatchExpressionAntecendent, RangeOpcode, SourceLocation},
	ProvidesCompilerDiagnostic, SourceSpan,
};
use log::*;
#[derive(Debug)]
pub enum Res {
	Err(miette::Report),
	GenericValue,
}
impl Expression {
	pub fn eval(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		scope_id: usize,
		ctx: &Box<LocalAnalyzerContext>,
	) -> miette::Result<Option<NumericConstant>> {
		let additional_ctx = AdditionalContext::new(
			ctx.nc_widths.clone(),
			ctx.ncs_to_be_exted.clone(),
			ctx.array_or_bus.clone(),
			ctx.casts.clone(),
		);
		let expr = self.eval_with_hirn(global_ctx, scope_id, &ctx.scope, Some(&additional_ctx));
		match expr {
			Ok(expression) => {
				let eval_ctx = hirn::design::EvalContext::without_assumptions(global_ctx.design.clone());
				let res = expression.eval(&eval_ctx).map_err(|err|{
					log::debug!("Error is {:?}", err);
					miette::Report::new(
						SemanticError::EvaluationError(err)
							.to_diagnostic_builder()
							.label(
								self.get_location(),
								"Error occured while evaluating value of this expression",
							)
							.build(),
					)
				})?;
				Ok(Some(NumericConstant::from_hirn_numeric_constant(res)))
			},
			Err(res) => match res {
				Res::Err(report) => Err(report),
				Res::GenericValue => Ok(None),
			},
		}
	}
	pub fn eval_with_hirn(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		scope_id: usize,
		scope: &ModuleImplementationScope,
		additional_ctx: Option<&AdditionalContext>,
	) -> Result<hirn::design::Expression, Res> {
		use self::Expression::*;
		match self {
			Number(num) => {
				if let Some(ncs) = additional_ctx {
					log::debug!("Location is {:?}", self.get_location());
					log::debug!("ncs.nc_widths is {:?}", ncs.ncs_to_be_exted);
					if let Some(loc) = ncs.ncs_to_be_exted.get(&self.get_location()) {
						debug!("Found a constant to be extended {:?}", loc);
						let constant = global_ctx.nc_table.get_by_key(&num.key).unwrap();
						let value: BigInt = constant.value.clone();
						let width_loc = scope.get_expression(*loc);
						let width = width_loc.expression.eval_with_hirn(
							global_ctx,
							width_loc.scope_id,
							scope,
							additional_ctx,
						)?;
						let signed = if let Some(nc) = ncs.nc_widths.get(&self.get_location()) {
							log::debug!("We will read sign!");
							match nc.signed {
								Some(s) => s,
								None => true,
							}
						}
						else {
							match constant.signed {
								Some(s) => s,
								None => true,
							}
						};
						log::debug!("Sign is {:?}", signed);
						let constant = hirn::design::Expression::Constant(
							hirn::design::NumericConstant::from_bigint(
								constant.value.clone(),
								if signed {
									hirn::design::SignalSignedness::Signed
								}
								else {
									hirn::design::SignalSignedness::Unsigned
								},
								max(value.bits() + if signed { 1 } else { 0 }, 1),
							)
							.unwrap(),
						);
						if signed {
							return Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::SignExtend {
								expr: Box::new(constant),
								width: Box::new(width),
							}));
						}
						else {
							return Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::ZeroExtend {
								expr: Box::new(constant),
								width: Box::new(width),
							}));
						}
					}
					if let Some(nc) = ncs.nc_widths.get(&num.location) {
						let signed = match nc.signed {
							Some(val) => val,
							None => true,
						};
						return Ok(hirn::design::Expression::Constant(
							hirn::design::NumericConstant::from_bigint(
								nc.value.clone(),
								match nc.signed {
									Some(val) => {
										if val {
											hirn::design::SignalSignedness::Signed
										}
										else {
											hirn::design::SignalSignedness::Unsigned
										}
									},
									None => hirn::design::SignalSignedness::Signed,
								},
								nc.width
									.unwrap_or(max(nc.value.bits() + if signed { 1 } else { 0 }, 1) as u32)
									.into(),
							)
							.unwrap(),
						));
					}
				}
				let constant = global_ctx.nc_table.get_by_key(&num.key).unwrap();
				let signed = match constant.signed {
					Some(s) => s,
					None => true,
				};
				let w = match constant.width.is_some() {
					true => constant.width.unwrap(),
					_ => max(constant.value.bits() + if signed { 1 } else { 0 }, 1) as u32,
				};
				debug!("Width is {:?}", w);
				let r = 	hirn::design::NumericConstant::from_bigint(
						constant.value.clone(),
						if signed {
							hirn::design::SignalSignedness::Signed
						}
						else {
							hirn::design::SignalSignedness::Unsigned
						},
						w.into(),
					).map_err(|err|{
						log::debug!("Error is {:?}", err);
						Res::Err(miette::Report::new(
							SemanticError::EvaluationError(err)
								.to_diagnostic_builder()
								.label(
									num.location,
									"Error while evaluating numeric constant",
								)
								.build(),
						))
					})?;
				Ok(hirn::design::Expression::Constant(r))
			},
			Identifier(id) => {
				let var = scope.get_variable(scope_id, &id.id).unwrap();
				use crate::analyzer::VariableKind::*;
				let nc =
					match &var.var.kind {
						Signal(_) => return Err(Res::Err(miette::Report::new(
							SemanticError::NonGenericTypeVariableInExpression
								.to_diagnostic_builder()
								.label(
									id.location,
									"This variable is used in expression but its value its not known at compile time",
								)
								.build(),
						))),
						Generic(generic) => match &generic.value {
							Some(val) => match val {
								BusWidth::Evaluated(nc) | BusWidth::EvaluatedLocated(nc, _) => {
									let mut new_nc = nc.clone();
									match generic.signedness {
										SignalSignedness::Signed(_) => new_nc.signed = Some(true),
										SignalSignedness::Unsigned(_) => new_nc.signed = Some(false),
										SignalSignedness::NoSignedness => (),
									}
									new_nc
								},
								_ => return Err(Res::GenericValue),
							},
							None => {
								if let crate::analyzer::Direction::Input(_) = &generic.direction {
									return Err(Res::GenericValue);
								}
								return Err(Res::Err(miette::Report::new(
							SemanticError::NonGenericTypeVariableInExpression
								.to_diagnostic_builder()
								.label(
									id.location,
									"This variable is used in expression but its value its not known at compile time",
								)
								.build(),
							)));
							},
						},
						ModuleInstance(_) => unreachable!(),
					};
				let signed = match nc.signed {
					Some(val) => val,
					None => true,
				};
				return Ok(hirn::design::Expression::Constant(
					hirn::design::NumericConstant::from_bigint(
						nc.value.clone(),
						match nc.signed {
							Some(val) => {
								if val {
									hirn::design::SignalSignedness::Signed
								}
								else {
									hirn::design::SignalSignedness::Unsigned
								}
							},
							None => hirn::design::SignalSignedness::Signed,
						},
						nc.width
							.unwrap_or(max(nc.value.bits() + if signed { 1 } else { 0 }, 1) as u32)
							.into(),
					)
					.unwrap(),
				));
			},
			ParenthesizedExpression(expr) => {
				expr.expression
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)
			},
			MatchExpression(match_expr) => {
				let def = match_expr.get_default().unwrap();
				let mut builder = hirn::design::Expression::new_conditional(def.expression.eval_with_hirn(
					global_ctx,
					scope_id,
					scope,
					additional_ctx,
				)?);
				for stmt in &match_expr.statements {
					let cond = match_expr
						.value
						.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
					match &stmt.antecedent {
						MatchExpressionAntecendent::Expression {
							expressions,
							location: _,
						} => {
							let val = stmt
								.expression
								.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
							for expr in expressions {
								builder = builder.branch(
									hirn::design::Expression::Binary(hirn::design::BinaryExpression {
										lhs: Box::new(cond.clone()),
										op: hirn::design::BinaryOp::Equal,
										rhs: Box::new(expr.eval_with_hirn(
											global_ctx,
											scope_id,
											scope,
											additional_ctx,
										)?),
									}),
									val.clone(),
								);
							}
						},
						MatchExpressionAntecendent::Default { location: _ } => (),
					};
				}
				Ok(builder.build())
			},
			ConditionalExpression(cond) => {
				let def = cond.get_default().unwrap();
				let mut builder = hirn::design::Expression::new_conditional(def.expression.eval_with_hirn(
					global_ctx,
					scope_id,
					scope,
					additional_ctx,
				)?);
				for stmt in &cond.statements {
					match &stmt.antecedent {
						MatchExpressionAntecendent::Expression {
							expressions,
							location: _,
						} => {
							let val = stmt
								.expression
								.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
							for expr in expressions {
								builder = builder.branch(
									expr.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?,
									val.clone(),
								);
							}
						},
						MatchExpressionAntecendent::Default { location: _ } => (),
					};
				}
				Ok(builder.build())
			},
			Tuple(_) => unreachable!(),
			TernaryExpression(ternary) => {
				let cond = ternary
					.condition
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				let true_branch = ternary
					.true_branch
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				let false_branch = ternary
					.false_branch
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				let builder = hirn::design::Expression::new_conditional(false_branch);
				Ok(builder.branch(cond, true_branch).build())
			},
			PostfixWithIndex(ind) => {
				if let Some(ctx) = additional_ctx {
					let is_array = ctx.array_or_bus.get(&self.get_location()).unwrap().clone();
					let index = ind.index.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
					if is_array {
						let mut expr = ind
							.expression
							.get_slice(global_ctx, scope_id, scope, additional_ctx)
							.unwrap();
						expr.indices.push(index);
						return Ok(expr.into());
					}
					else {
						let expr = ind
							.expression
							.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
						return Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::BitSelect {
							expr: Box::new(expr),
							index: Box::new(index),
						}));
					}
				}
				unreachable!()
			},
			PostfixWithRange(range) => {
				let expr = range
					.expression
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				let mut msb = range
					.range
					.rhs
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				let lsb = range
					.range
					.lhs
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				use RangeOpcode::*;
				let signed_one = scope.ext_signedness.get(&self.get_location()).unwrap();
				let one = match signed_one {
					true => hirn::design::Expression::Constant(hirn::design::NumericConstant::new_signed(1.into())),
					false => hirn::design::Expression::Constant(hirn::design::NumericConstant::new_unsigned(1.into())),
				};
				match range.range.code {
					Colon => (),
					PlusColon => msb = msb + lsb.clone() - one,
					ColonLessThan => msb = msb - one,
				}
				Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::BusSelect {
					expr: Box::new(expr),
					msb: Box::new(msb),
					lsb: Box::new(lsb),
				}))
			},
			PostfixWithArgs(function) => {
				let func_name = global_ctx.id_table.get_value(&function.id);
				match func_name.as_str() {
					"trunc" => {
						let expr = function.argument_list.first().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						let width = scope.widths.get(&function.location).unwrap().clone(); //FIXME
						log::debug!("Width is {:?}", width);
						if let Some(loc) = width.get_location() {
							let op = hirn::design::BuiltinOp::BusSelect {
								expr: Box::new(expr),
								msb: Box::new(
									scope.get_expression(loc).expression.eval_with_hirn(
										global_ctx,
										scope_id,
										scope,
										additional_ctx,
									)? - hirn::design::Expression::Constant(hirn::design::NumericConstant::new_signed(
										BigInt::from(1),
									)),
								),
								lsb: Box::new(hirn::design::Expression::Constant(
									hirn::design::NumericConstant::new_signed(BigInt::from(0)),
								)),
							};
							return Ok(hirn::design::Expression::Builtin(op));
						}
						if let Some(val) = &width.get_value() {
							let op = hirn::design::BuiltinOp::BusSelect {
								expr: Box::new(expr),
								msb: Box::new(hirn::design::Expression::Constant(
									hirn::design::NumericConstant::new_unsigned(val.clone() - 1),
								)),
								lsb: Box::new(hirn::design::Expression::Constant(
									hirn::design::NumericConstant::new_unsigned(BigInt::from(0)),
								)),
							};
							return Ok(hirn::design::Expression::Builtin(op));
						}
						unreachable!()
					},
					"zeros" => {
						let expr = hirn::design::NumericConstant::from_bigint(
							0.into(),
							hirn::design::SignalSignedness::Unsigned,
							1,
						)
						.unwrap(); // FIXME
						let count = function.argument_list.first().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						debug!("Count is {:?}", count);
						debug!("Expr is {:?}", expr);
						Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Replicate {
							expr: Box::new(expr.into()),
							count: Box::new(count),
						}))
					},
					"ones" => {
						let expr = hirn::design::NumericConstant::from_bigint(
							1.into(),
							hirn::design::SignalSignedness::Unsigned,
							1,
						)
						.unwrap(); // FIXME
						let count = function.argument_list.first().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Replicate {
							expr: Box::new(expr.into()),
							count: Box::new(count),
						}))
					},
					"zext" | "ext" | "sext" => {
						let expr = function.argument_list.first().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						let width = scope.widths.get(&function.location).unwrap().clone(); //FIXME
						log::debug!("Width is {:?}", width);
						if let Some(loc) = width.get_location() {
							let op = match func_name.as_str() {
								"zext" => hirn::design::BuiltinOp::ZeroExtend {
									expr: Box::new(expr),
									width: Box::new(scope.get_expression(loc).expression.eval_with_hirn(
										global_ctx,
										scope_id,
										scope,
										additional_ctx,
									)?),
								},
								"ext" => match scope.ext_signedness.get(&self.get_location()).unwrap() {
									true => hirn::design::BuiltinOp::SignExtend {
										expr: Box::new(expr),
										width: Box::new(scope.get_expression(loc).expression.eval_with_hirn(
											global_ctx,
											scope_id,
											scope,
											additional_ctx,
										)?),
									},
									false => hirn::design::BuiltinOp::ZeroExtend {
										expr: Box::new(expr),
										width: Box::new(scope.get_expression(loc).expression.eval_with_hirn(
											global_ctx,
											scope_id,
											scope,
											additional_ctx,
										)?),
									},
								},
								"sext" => hirn::design::BuiltinOp::SignExtend {
									expr: Box::new(expr),
									width: Box::new(scope.get_expression(loc).expression.eval_with_hirn(
										global_ctx,
										scope_id,
										scope,
										additional_ctx,
									)?),
								},
								_ => unreachable!(),
							};
							return Ok(hirn::design::Expression::Builtin(op));
						}
						if let Some(val) = &width.get_value() {
							let op = match func_name.as_str() {
								"zext" => hirn::design::BuiltinOp::ZeroExtend {
									expr: Box::new(expr),
									width: Box::new(hirn::design::Expression::Constant(
										hirn::design::NumericConstant::new_unsigned(val.clone()),
									)),
								},
								"ext" => match scope.ext_signedness.get(&self.get_location()).unwrap() {
									true => hirn::design::BuiltinOp::SignExtend {
										expr: Box::new(expr),
										width: Box::new(hirn::design::Expression::Constant(
											hirn::design::NumericConstant::new_unsigned(val.clone()),
										)),
									},
									false => hirn::design::BuiltinOp::ZeroExtend {
										expr: Box::new(expr),
										width: Box::new(hirn::design::Expression::Constant(
											hirn::design::NumericConstant::new_unsigned(val.clone()),
										)),
									},
								},
								"sext" => hirn::design::BuiltinOp::SignExtend {
									expr: Box::new(expr),
									width: Box::new(hirn::design::Expression::Constant(
										hirn::design::NumericConstant::new_unsigned(val.clone()),
									)),
								},
								_ => unreachable!(),
							};
							return Ok(hirn::design::Expression::Builtin(op));
						}
						unreachable!()
					},
					"join" => {
						let mut exprs = Vec::new();
						for expr in &function.argument_list {
							exprs.push(expr.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?);
						}
						Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Join(exprs)))
					},
					"rep" => {
						let expr = function.argument_list.first().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						let count = function.argument_list.last().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Builtin(hirn::design::BuiltinOp::Replicate {
							expr: Box::new(expr),
							count: Box::new(count),
						}))
					},
					"fold_or" => {
						let expr = function.argument_list.first().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Unary(hirn::design::UnaryExpression {
							op: hirn::design::UnaryOp::ReductionOr,
							operand: Box::new(expr),
						}))
					},
					"fold_xor" => {
						let expr = function.argument_list.first().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Unary(hirn::design::UnaryExpression {
							op: hirn::design::UnaryOp::ReductionXor,
							operand: Box::new(expr),
						}))
					},
					"fold_and" => {
						let expr = function.argument_list.first().unwrap().eval_with_hirn(
							global_ctx,
							scope_id,
							scope,
							additional_ctx,
						)?;
						Ok(hirn::design::Expression::Unary(hirn::design::UnaryExpression {
							op: hirn::design::UnaryOp::ReductionAnd,
							operand: Box::new(expr),
						}))
					},
					_ => unreachable!(),
				}
			},
			PostfixWithId(_) => report_not_allowed_expression_eval(self.get_location(), "PostfixWithId"),
			UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				let operand = unary
					.expression
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				match unary.code {
					LogicalNot => Ok(!operand),
					BitwiseNot => Ok(hirn::design::Expression::Unary(hirn::design::UnaryExpression {
						op: hirn::design::UnaryOp::BitwiseNot,
						operand: Box::new(operand),
					})),
					Minus => Ok(-operand),
					Plus => Ok(operand),
				}
			},
			UnaryCastExpression(unary_cast) => {
				let src = unary_cast
					.expression
					.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				let cast = additional_ctx.unwrap().casts.get(&self.get_location()).unwrap();
				use SignalSensitivity::*;
				let sensitivity = match &cast.dest_sensitivity {
					Async(_) => Some(hirn::design::SignalSensitivity::Async),
					Comb(list, _) => {
						let mut new_list = hirn::design::ClockSensitivityList::new_empty();
						for edge in &list.list {
							let id = scope.get_api_id_by_internal_id(edge.clock_signal).unwrap();
							let new_edge = hirn::design::EdgeSensitivity {
								clock_signal: id,
								on_rising: edge.on_rising,
							};
							new_list.push(new_edge);
						}
						Some(hirn::design::SignalSensitivity::Comb(new_list))
					},
					Sync(list, _) => {
						let mut new_list = hirn::design::ClockSensitivityList::new_empty();
						for edge in &list.list {
							let id = scope.get_api_id_by_internal_id(edge.clock_signal).unwrap();
							let new_edge = hirn::design::EdgeSensitivity {
								clock_signal: id,
								on_rising: edge.on_rising,
							};
							new_list.push(new_edge);
						}
						Some(hirn::design::SignalSensitivity::Sync(new_list))
					},
					Clock(..) => Some(hirn::design::SignalSensitivity::Clock),
					Const(_) => Some(hirn::design::SignalSensitivity::Const),
					NoSensitivity => None,
				};
				use SignalSignedness::*;
				let signedness = match &cast.dest_signedness {
					Signed(_) => Some(hirn::design::SignalSignedness::Signed),
					Unsigned(_) => Some(hirn::design::SignalSignedness::Unsigned),
					NoSignedness => None,
				};
				Ok(hirn::design::Expression::Cast(hirn::design::CastExpression {
					src: Box::new(src),
					signedness,
					sensitivity,
				}))
			},
			BinaryExpression(binop) => {
				use crate::parser::ast::BinaryOpcode::*;
				let lhs = binop.lhs.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				let rhs = binop.rhs.eval_with_hirn(global_ctx, scope_id, scope, additional_ctx)?;
				match binop.code {
					Multiplication => Ok(lhs * rhs),
					Division => Ok(lhs / rhs),
					Addition => Ok(lhs + rhs),
					Subtraction => Ok(lhs - rhs),
					Modulo => Ok(lhs % rhs),
					Equal => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::Equal,
						rhs: Box::new(rhs),
					})),
					NotEqual => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::NotEqual,
						rhs: Box::new(rhs),
					})),
					LShift => Ok(lhs << rhs),
					RShift => Ok(lhs >> rhs),
					BitwiseAnd => Ok(lhs & rhs),
					BitwiseOr => Ok(lhs | rhs),
					BitwiseXor => Ok(lhs ^ rhs),
					Less => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::Less,
						rhs: Box::new(rhs),
					})),
					Greater => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::Greater,
						rhs: Box::new(rhs),
					})),
					LessEqual => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::LessEqual,
						rhs: Box::new(rhs),
					})),
					GreaterEqual => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::GreaterEqual,
						rhs: Box::new(rhs),
					})),
					LogicalAnd => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::LogicalAnd,
						rhs: Box::new(rhs),
					})),
					LogicalOr => Ok(hirn::design::Expression::Binary(hirn::design::BinaryExpression {
						lhs: Box::new(lhs),
						op: hirn::design::BinaryOp::LogicalOr,
						rhs: Box::new(rhs),
					})),
				}
			},
		}
	}
}
fn report_not_allowed_expression_eval(span: SourceSpan, expr_name: &str) -> Result<hirn::design::Expression, Res> {
	Err(Res::Err(miette::Report::new(
		SemanticError::ExpressionNotAllowedInNonGenericModuleDeclaration
			.to_diagnostic_builder()
			.label(
				span,
				format!(
					"This {} expression is not allowed in non-generic module declaration",
					expr_name
				)
				.as_str(),
			)
			.build(),
	)))
}
