use crate::parser::ast::Expression;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for Expression {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use Expression::*;
		match self {
			Number { key, .. } => ctx.write(format!("{}", &ctx.get_numeric_constant(*key).to_string()).as_str()),
			Identifier { id, .. } => ctx.write(format!("{}", &ctx.get_id(*id)).as_str()),
			ParenthesizedExpression { expression, .. } => {
				ctx.write("(")?;
				expression.pretty_print(ctx)?;
				ctx.write(")")
			},
			MatchExpression { value, statements, .. } => {
				ctx.write("match (")?;
				value.pretty_print(ctx)?;
				ctx.write(")")?;
				ctx.increase_indent();
				ctx.writeln(" {")?;
				for statement in statements {
					statement.pretty_print(ctx)?;
				}
				ctx.decrease_indent();
				ctx.write_indent("}")
			},
			ConditionalExpression { statements, .. } => {
				ctx.write("conditional")?;
				ctx.increase_indent();
				ctx.writeln(" {")?;
				for statement in statements {
					statement.pretty_print(ctx)?;
				}
				ctx.decrease_indent();
				ctx.writeln("}")
			},
			Tuple { expressions, .. } => {
				ctx.write("(")?;
				for (i, expression) in expressions.iter().enumerate() {
					expression.pretty_print(ctx)?;
					if i != expressions.len() - 1 {
						ctx.write(", ")?;
					}
				}
				ctx.write(")")
			},
			TernaryExpression {
				condition,
				true_branch,
				false_branch,
				..
			} => {
				condition.pretty_print(ctx)?;
				ctx.write(" ? ")?;
				true_branch.pretty_print(ctx)?;
				ctx.write(" : ")?;
				false_branch.pretty_print(ctx)
			},
			PostfixWithIndex { expression, index, .. } => {
				expression.pretty_print(ctx)?;
				ctx.write("[")?;
				index.pretty_print(ctx)?;
				ctx.write("]")
			},
			PostfixWithRange { expression, range, .. } => {
				expression.pretty_print(ctx)?;
				ctx.write("[")?;
				range.pretty_print(ctx)?;
				ctx.write("]")
			},
			PostfixWithArgs {
				expression,
				argument_list,
				..
			} => {
				expression.pretty_print(ctx)?;
				ctx.write("(")?;
				for (i, expression) in argument_list.iter().enumerate() {
					expression.pretty_print(ctx)?;
					if i != argument_list.len() - 1 {
						ctx.write(", ")?;
					}
				}
				ctx.write(")")
			},
			// PostfixEmptyCall { expression, .. } => {
			// 	expression.pretty_print(ctx)?;
			// 	ctx.write("()")
			// },
			PostfixWithId { expression, id, .. } => {
				expression.pretty_print(ctx)?;
				ctx.write(format!(".{}", ctx.get_id(*id)).as_str())
			},
			UnaryOperatorExpression { expression, code, .. } => {
				ctx.write(format!("{:?}", code).as_str())?;
				expression.pretty_print(ctx)
			},
			UnaryCastExpression {
				type_name, expression, ..
			} => {
				ctx.write("(")?;
				type_name.pretty_print(ctx)?;
				ctx.write(")")?;
				expression.pretty_print(ctx)
			},
			// RangeExpression { lhs, rhs, code, .. } => {
			// 	ctx.write("[")?;
			// 	lhs.pretty_print(ctx)?;
			// 	ctx.write(format!(" {:?} ", code).as_str())?;
			// 	rhs.pretty_print(ctx)?;
			// 	ctx.write("]")
			// },
			BinaryExpression { lhs, rhs, code, .. } => {
				lhs.pretty_print(ctx)?;
				ctx.write(format!(" {:?} ", code).as_str())?;
				rhs.pretty_print(ctx)
			},
			Error => ctx.write("error"),
		}
	}
}
