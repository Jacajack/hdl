mod binary_expression;
mod conditional_expression;
mod identifier;
mod match_expression;
mod number;
mod parenthesized;
mod postfix_with_args;
mod postfix_with_id;
mod postfix_with_index;
mod postfix_with_range;
mod pretty_printable;
mod ternary_expression;
mod tuple;
mod unary_cast_expression;
mod unary_operator_expression;
use crate::analyzer::{ModuleImplementationScope, SemanticError, EdgeSensitivity, Signal, VariableKind, GenericVariable, GenericVariableKind};
use crate::core::id_table;
use crate::lexer::{IdTableKey, NumericConstantBase};
use crate::parser::ast::{opcodes::*, MatchExpressionStatement, RangeExpression, SourceLocation, TypeName};
use crate::{ProvidesCompilerDiagnostic, SourceSpan};
pub use binary_expression::BinaryExpression;
pub use conditional_expression::ConditionalExpression;
pub use identifier::Identifier;
pub use match_expression::MatchExpression;
use num_traits::Zero;
pub use number::Number;
pub use parenthesized::ParenthesizedExpression;
pub use postfix_with_args::PostfixWithArgs;
pub use postfix_with_id::PostfixWithId;
pub use postfix_with_index::PostfixWithIndex;
pub use postfix_with_range::PostfixWithRange;
use std::fmt::{Debug, Error, Formatter};
pub use ternary_expression::TernaryExpression;
pub use tuple::Tuple;
pub use unary_cast_expression::UnaryCastExpression;
pub use unary_operator_expression::UnaryOperatorExpression;

use crate::lexer::NumericConstant;
use num_bigint::{BigInt, Sign};
#[derive(serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq)]
pub enum Expression {
	Number(Number),
	Identifier(Identifier), // takes in part in two-sided type deduction  as a rhs
	ParenthesizedExpression(ParenthesizedExpression),
	MatchExpression(MatchExpression),
	ConditionalExpression(ConditionalExpression),
	Tuple(Tuple),
	TernaryExpression(TernaryExpression),
	PostfixWithIndex(PostfixWithIndex), // takes in part in two-sided type deduction as a rhs
	PostfixWithRange(PostfixWithRange), // takes in part in two-sided type deduction as a rhs
	PostfixWithArgs(PostfixWithArgs), // takes in part in two-sided type deduction as a rhs
	PostfixWithId(PostfixWithId), // takes in part in two-sided type deduction as a rhs
	UnaryOperatorExpression(UnaryOperatorExpression),
	UnaryCastExpression(UnaryCastExpression),
	BinaryExpression(BinaryExpression),
}

impl Debug for Expression {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::Expression::*;
		match &self {
			Number(number) => write!(fmt, "{:?}", number.key),
			Identifier(_) => write!(fmt, "foo"),
			ParenthesizedExpression(expr) => write!(fmt, "({:?})", *expr.expression),
			BinaryExpression(binop) => {
				write!(fmt, "({:?} {:?} {:?})", binop.lhs, binop.code, binop.rhs)
			},
			TernaryExpression(teranary) => {
				write!(
					fmt,
					"({:?} ? {:?} : {:?})",
					teranary.condition, teranary.true_branch, teranary.false_branch
				)
			},
			UnaryOperatorExpression(unary) => write!(fmt, "{:?}{:?}", unary.code, unary.expression),
			PostfixWithId(postfix) => write!(fmt, "({:?}.{:?})", postfix.expression, postfix.id),
			PostfixWithIndex(postfix) => write!(fmt, "({:?}[{:?}])", postfix.expression, postfix.index),
			PostfixWithRange(postfix) => write!(fmt, "({:?}{:?})", postfix.expression, postfix.range),
			PostfixWithArgs(postfix) => {
				write!(fmt, "({:?}(", postfix.id)?;
				for arg in postfix.argument_list.iter() {
					write!(fmt, "{:?}, ", arg)?;
				}
				write!(fmt, "))")
			},
			UnaryCastExpression(unary) => write!(fmt, "(({:?}){:?})", unary.type_name, unary.expression),
			Tuple(tuple) => {
				write!(fmt, "{{")?;
				for expr in tuple.expressions.iter() {
					write!(fmt, "{:?},", expr)?;
				}
				write!(fmt, "}}")
			},
			MatchExpression(match_expr) => {
				writeln!(fmt, "match({:?}){{", match_expr.value)?;
				for s in match_expr.statements.iter() {
					writeln!(fmt, "{:?},", s)?;
				}
				write!(fmt, "}}")
			},
			ConditionalExpression(cond) => {
				writeln!(fmt, "conditional{{")?;
				for s in cond.statements.iter() {
					writeln!(fmt, "{:?},", s)?;
				}
				write!(fmt, "}}")
			},
		}
	}
}
impl SourceLocation for Expression {
	fn get_location(&self) -> SourceSpan {
		use self::Expression::*;
		match self {
			Number(number) => number.location,
			Identifier(ident) => ident.location,
			ParenthesizedExpression(expr) => expr.location,
			MatchExpression(match_expr) => match_expr.location,
			ConditionalExpression(cond) => cond.location,
			Tuple(tuple) => tuple.location,
			TernaryExpression(ternary) => ternary.location,
			PostfixWithIndex(postfix) => postfix.location,
			PostfixWithRange(postfix) => postfix.location,
			PostfixWithArgs(postfix) => postfix.location,
			PostfixWithId(postfix) => postfix.location,
			UnaryOperatorExpression(unary) => unary.location,
			UnaryCastExpression(unary) => unary.location,
			BinaryExpression(binop) => binop.location,
		}
	}
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
	pub value: Option<BigInt>,
	pub signed: Option<bool>,
	pub width: Option<u32>,
	pub dimensions: Vec<BigInt>,
	// sensitivity ?
}
impl Value {
	pub fn new_from_constant(constant: &NumericConstant) -> Self {
		Self {
			value: Some(constant.value.clone()),
			signed: constant.signed,
			width: constant.width,
			dimensions: vec![],
		}
	}
}
impl Expression {
	/// deprecated, not used
	pub fn get_name_sync_or_comb(&self) -> miette::Result<IdTableKey> {
		use self::Expression::*;
		match self {
			Identifier(ident) => Ok(ident.id),
			UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					LogicalNot => unary.expression.get_name_sync_or_comb(),
					_ => {
						return Err(miette::Report::new(
							SemanticError::ForbiddenExpressionInSyncOrComb
								.to_diagnostic_builder()
								.label(
									unary.location,
									"This expression is not allowed in sync or comb qualifier",
								)
								.build(),
						))
					},
				}
			},
			_ => {
				return Err(miette::Report::new(
					SemanticError::ForbiddenExpressionInSyncOrComb
						.to_diagnostic_builder()
						.label(
							self.get_location(),
							"This expression is not allowed in sync or comb qualifier",
						)
						.build(),
				))
			},
		}
	}
	pub fn create_edge_sensitivity(&self) -> miette::Result<EdgeSensitivity>{
		use self::Expression::*;
		match self{
    		Identifier(id) => Ok(EdgeSensitivity{clock_signal: id.id, on_rising: true, location: id.location}),
    		UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					LogicalNot => unary.expression.create_edge_sensitivity().map(|mut edge| {edge.on_rising = !edge.on_rising; edge}),
					_ => {
						return Err(miette::Report::new(
							SemanticError::ForbiddenExpressionInSyncOrComb
								.to_diagnostic_builder()
								.label(
									unary.location,
									"This expression is not allowed in sync or comb qualifier",
								)
								.build(),
						))
					},
				}
			},
			_ => return Err(miette::Report::new(
				SemanticError::ForbiddenExpressionInSyncOrComb
					.to_diagnostic_builder()
					.label(
						self.get_location(),
						"This expression is not allowed in sync or comb qualifier",
					)
					.build(),
			))
	}
	}
	pub fn evaluate(
		&self,
		nc_table: &crate::lexer::NumericConstantTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
	) -> miette::Result<NumericConstant> {
		match self {
			Expression::Number(nc_key) => {
				let constant = nc_table.get_by_key(&nc_key.key).unwrap();
				Ok(constant.clone())
			},
			Expression::Identifier(id) => {
				let var =  match scope.get_variable(scope_id, &id.id) {
        			Some(var) => var,
        			None => return Err(miette::Report::new(
						SemanticError::VariableNotDeclared
							.to_diagnostic_builder()
							.label(id.location, "This variable is not defined in this scope")
							.build(),
					)),
    			};
				use crate::analyzer::VariableKind::*;
				match &var.var.kind {
        			Signal(_) => Err(miette::Report::new(
						SemanticError::NonGenericTypeVariableInExpression
							.to_diagnostic_builder()
							.label(
								id.location,
								"This variable is used in expression but its value its not known at compile time",
							)
							.build(),
					)),
        			Generic(generic) => {
						match &generic.value {
        					Some(val) => Ok(val.clone()),
        					None => Err(miette::Report::new(
								SemanticError::NonGenericTypeVariableInExpression
									.to_diagnostic_builder()
									.label(
										id.location,
										"This variable is used in expression but its value its not known at compile time",
									)
									.build(),
							)),
    					}
					},
    			}
				
			},
			Expression::ParenthesizedExpression(expr) => expr.expression.evaluate(nc_table, scope_id, scope),
			Expression::MatchExpression(expr) => report_not_allowed_expression(expr.location, "match"),
			Expression::ConditionalExpression(expr) => report_not_allowed_expression(expr.location, "conditional"),
			Expression::Tuple(_) => unreachable!(),
			Expression::TernaryExpression(tern) => Ok(
				if tern.condition.evaluate(nc_table, scope_id, scope)?.value != BigInt::from(0) {
					tern.true_branch.evaluate(nc_table, scope_id, scope)?
				}
				else {
					tern.false_branch.evaluate(nc_table, scope_id, scope)?
				},
			),
			Expression::PostfixWithIndex(expr) => report_not_allowed_expression(expr.location, "index"),
			Expression::PostfixWithRange(expr) => report_not_allowed_expression(expr.location, "range"), // nie bedzie
			Expression::PostfixWithArgs(_) => todo!(),                                                   // szerokość busa
			Expression::PostfixWithId(expr) => report_not_allowed_expression(expr.location, "postfix with id"), // nie bedzie
			Expression::UnaryOperatorExpression(unary) => {
				use crate::parser::ast::UnaryOpcode::*;
				match unary.code {
					LogicalNot => Ok(
						if unary.expression.evaluate(nc_table, scope_id, scope)?.value == BigInt::from(0) {
							NumericConstant::new(BigInt::from(1), None, None, Some(NumericConstantBase::Boolean))
						}
						else {
							NumericConstant::new(BigInt::from(0), None, None, Some(NumericConstantBase::Boolean))
						},
					),
					BitwiseNot => Ok(NumericConstant::new_from_unary(
						unary.expression.evaluate(nc_table, scope_id, scope)?,
						|e| !e,
					)),
					Minus => {
						let other = unary.expression.evaluate(nc_table, scope_id, scope)?;
						if let Some(false) = other.signed {
							// report an error
						}
						Ok(NumericConstant::new_from_unary(other, |e| -e))
					},
					Plus => Ok(NumericConstant::new_from_unary(
						unary.expression.evaluate(nc_table, scope_id, scope)?,
						|e| e * (-1),
					)),
				}
			},
			Expression::UnaryCastExpression(expr) => report_not_allowed_expression(expr.location, "unary cast"), // nie bedzie
			Expression::BinaryExpression(binop) => {
				use crate::parser::ast::BinaryOpcode::*;
				match binop.code {
					Multiplication => Ok(NumericConstant::new_from_binary(
						binop.lhs.evaluate(nc_table, scope_id, scope)?,
						binop.rhs.evaluate(nc_table, scope_id, scope)?,
						|e1, e2| e1 * e2,
					)),
					Division => {
						let rhs = binop.rhs.evaluate(nc_table, scope_id, scope)?;
						if rhs.value.is_zero() {
							return Err(miette::Report::new(
								SemanticError::DivisionByZero
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Division by zero is not allowed")
									.build(),
							));
						}
						Ok(NumericConstant::new_from_binary(
							binop.lhs.evaluate(nc_table, scope_id, scope)?,
							rhs,
							|e1, e2| e1 / e2,
						))
					},
					Addition => Ok(NumericConstant::new_from_binary(
						binop.lhs.evaluate(nc_table, scope_id, scope)?,
						binop.rhs.evaluate(nc_table, scope_id, scope)?,
						|e1, e2| e1 + e2,
					)),
					Subtraction => Ok(NumericConstant::new_from_binary(
						binop.lhs.evaluate(nc_table, scope_id, scope)?,
						binop.rhs.evaluate(nc_table, scope_id, scope)?,
						|e1, e2| e1 - e2,
					)),
					Modulo => {
						let rhs = binop.rhs.evaluate(nc_table, scope_id, scope)?;
						if rhs.value.is_zero() {
							return Err(miette::Report::new(
								SemanticError::DivisionByZero
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "It is not allowed to modulo by zero")
									.build(),
							));
						}
						Ok(NumericConstant::new_from_binary(
							binop.lhs.evaluate(nc_table, scope_id, scope)?,
							rhs,
							|e1, e2| e1 / e2,
						))
					},
					Equal => Ok(
						if binop.lhs.evaluate(nc_table, scope_id, scope)?.value
							== binop.rhs.evaluate(nc_table, scope_id, scope)?.value
						{
							NumericConstant::from_u64(1, Some(1), Some(false), Some(NumericConstantBase::Boolean))
						}
						else {
							NumericConstant::from_u64(0, Some(1), Some(false), Some(NumericConstantBase::Boolean))
						},
					),
					NotEqual => Ok(
						if binop.lhs.evaluate(nc_table, scope_id, scope)?.value
							!= binop.rhs.evaluate(nc_table, scope_id, scope)?.value
						{
							NumericConstant::from_u64(1, Some(1), Some(false), Some(NumericConstantBase::Boolean))
						}
						else {
							NumericConstant::from_u64(0, Some(1), Some(false), Some(NumericConstantBase::Boolean))
						},
					),
					LShift => {
						let mut lhs = binop.lhs.evaluate(nc_table, scope_id, scope)?;
						let rhs = binop.rhs.evaluate(nc_table, scope_id, scope)?;
						if rhs.value.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						}
						else {
							let mut i = BigInt::from(0);
							while i < rhs.value {
								lhs.value = lhs.value << 1;
								i += BigInt::from(1);
							}
							Ok(lhs)
						}
					},
					RShift => {
						let mut lhs = binop.lhs.evaluate(nc_table, scope_id, scope)?;
						let rhs = binop.rhs.evaluate(nc_table, scope_id, scope)?;
						if rhs.value.sign() == Sign::Minus {
							return Err(miette::Report::new(
								SemanticError::ShiftByNegativeNumber
									.to_diagnostic_builder()
									.label(binop.rhs.get_location(), "Shift by negative number is not allowed")
									.build(),
							));
						}
						else {
							let mut i = BigInt::from(0);
							while i < rhs.value {
								lhs.value = lhs.value >> 1;
								i += BigInt::from(1);
							}
							Ok(lhs)
						}
					},
					BitwiseAnd => Ok(NumericConstant::new_from_binary(
						binop.lhs.evaluate(nc_table, scope_id, scope)?,
						binop.rhs.evaluate(nc_table, scope_id, scope)?,
						|e1, e2| e1 & e2,
					)),
					BitwiseOr => Ok(NumericConstant::new_from_binary(
						binop.lhs.evaluate(nc_table, scope_id, scope)?,
						binop.rhs.evaluate(nc_table, scope_id, scope)?,
						|e1, e2| e1 | e2,
					)),
					BitwiseXor => Ok(NumericConstant::new_from_binary(
						binop.lhs.evaluate(nc_table, scope_id, scope)?,
						binop.rhs.evaluate(nc_table, scope_id, scope)?,
						|e1, e2| e1 ^ e2,
					)),
					Less => Ok(
						if binop.lhs.evaluate(nc_table, scope_id, scope)?.value
							< binop.rhs.evaluate(nc_table, scope_id, scope)?.value
						{
							NumericConstant::new_true()
						}
						else {
							NumericConstant::new_false()
						},
					),
					Greater => Ok(
						if binop.lhs.evaluate(nc_table, scope_id, scope)?.value
							> binop.rhs.evaluate(nc_table, scope_id, scope)?.value
						{
							NumericConstant::new_true()
						}
						else {
							NumericConstant::new_false()
						},
					),
					LessEqual => Ok(
						if binop.lhs.evaluate(nc_table, scope_id, scope)?.value
							<= binop.rhs.evaluate(nc_table, scope_id, scope)?.value
						{
							NumericConstant::new_true()
						}
						else {
							NumericConstant::new_false()
						},
					),
					GreaterEqual => Ok(
						if binop.lhs.evaluate(nc_table, scope_id, scope)?.value
							>= binop.rhs.evaluate(nc_table, scope_id, scope)?.value
						{
							NumericConstant::new_true()
						}
						else {
							NumericConstant::new_false()
						},
					),
					LogicalAnd => Ok(
						if binop.lhs.evaluate(nc_table, scope_id, scope)?.value != BigInt::from(0)
							&& binop.rhs.evaluate(nc_table, scope_id, scope)?.value != BigInt::from(0)
						{
							NumericConstant::new_true()
						}
						else {
							NumericConstant::new_false()
						},
					),
					LogicalOr => Ok(
						if binop.lhs.evaluate(nc_table, scope_id, scope)?.value != BigInt::from(0)
							|| binop.rhs.evaluate(nc_table, scope_id, scope)?.value != BigInt::from(0)
						{
							NumericConstant::new_true()
						}
						else {
							NumericConstant::new_false()
						},
					),
				}
			},
		}
	}
	pub fn evaluate_type(&self,
		nc_table: &crate::lexer::NumericConstantTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
	) -> miette::Result<Signal> {
		use Expression::*;
		match self {
    		Number(num) => {
				let key = &num.key;
				let constant = nc_table.get_by_key(key).unwrap();
				Ok(Signal::new_from_constant(constant, num.location))
			},
    		Identifier(_) => todo!(),
    		ParenthesizedExpression(expr) => expr.expression.evaluate_type(nc_table, scope_id, scope),
    		MatchExpression(_) => todo!(),
    		ConditionalExpression(_) => todo!(),
    		Tuple(_) => todo!(),
    		TernaryExpression(ternary) => {
				let type_first = ternary.true_branch.evaluate_type(nc_table, scope_id, scope)?;
				let type_second = ternary.false_branch.evaluate_type(nc_table, scope_id, scope)?;
				let type_condition = ternary.condition.evaluate_type(nc_table, scope_id, scope)?;
				Ok(type_first) // FIXME
			},
    		PostfixWithIndex(_) => todo!(),
    		PostfixWithRange(_) => todo!(),
    		PostfixWithArgs(_) => todo!(),
    		PostfixWithId(_) => todo!(),
    		UnaryOperatorExpression(_) => todo!(),
    		UnaryCastExpression(_) => todo!(),
    		BinaryExpression(_) => todo!(),
		}
	}
	//pub fn evaluate(
	//	&self,
	//	scope: ModuleImplementationScope,
	//	nc_table: &crate::lexer::NumericConstantTable,
	//	id_table: &id_table::IdTable,
	//) -> miette::Result<Value> {
	//	use self::Expression::*;
	//	match self {
	//		Number(num) => Ok(Value::new_from_constant(nc_table.get_by_key(&num.key).unwrap())),
	//		Identifier(_) => todo!(),
	//		ParenthesizedExpression(expr) => expr.expression.evaluate(scope, nc_table, id_table),
	//		MatchExpression(_) => todo!(),
	//		ConditionalExpression(_) => todo!(),
	//		Tuple(_) => todo!(),
	//		TernaryExpression(_) => todo!(),
	//		PostfixWithIndex(_) => todo!(),
	//		PostfixWithRange(_) => todo!(),
	//		PostfixWithArgs(_) => todo!(),
	//		PostfixWithId(_) => todo!(),
	//		UnaryOperatorExpression(_) => todo!(),
	//		UnaryCastExpression(_) => todo!(),
	//		BinaryExpression(_) => todo!(),
	//	}
	//}
	pub fn can_create_binding_as_rhs(&self) -> bool {
		use Expression::*;
		match &self {
    		Number(_) => false,
    		Identifier(_) => true,
    		ParenthesizedExpression(expr) => expr.expression.can_create_binding_as_rhs(),
    		MatchExpression(_) => todo!(),
    		ConditionalExpression(_) => todo!(),
    		Tuple(_) => false,
    		TernaryExpression(_) => false,
    		PostfixWithIndex(_) => true,
    		PostfixWithRange(_) => true,
    		PostfixWithArgs(_) => true,
    		PostfixWithId(_) => true,
    		UnaryOperatorExpression(_) => true,
    		UnaryCastExpression(_) => false,
    		BinaryExpression(_) => false,
		}
	}
}

fn report_not_allowed_expression(span: SourceSpan, expr_name: &str) -> miette::Result<NumericConstant> {
	Err(miette::Report::new(
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
	))
}
