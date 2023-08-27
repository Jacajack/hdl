use crate::parser::ast::Expression;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for Expression {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use Expression::*;
		match self {
			Number(num) => ctx.write(ctx.get_numeric_constant(num.key).to_pretty_string().as_str()),
			Identifier(ident) => ctx.write((&ctx.get_id(ident.id)).to_string().as_str()),
			ParenthesizedExpression(expr) => {
				ctx.write("(")?;
				expr.expression.pretty_print(ctx)?;
				ctx.write(")")
			},
			MatchExpression(match_expr) => {
				ctx.write("match (")?;
				match_expr.value.pretty_print(ctx)?;
				ctx.write(")")?;
				ctx.increase_indent();
				ctx.writeln(" {")?;
				for statement in &match_expr.statements {
					statement.pretty_print(ctx)?;
				}
				ctx.decrease_indent();
				ctx.write_indent("}")
			},
			ConditionalExpression(cond) => {
				ctx.write("conditional")?;
				ctx.increase_indent();
				ctx.writeln(" {")?;
				for statement in &cond.statements {
					statement.pretty_print(ctx)?;
				}
				ctx.decrease_indent();
				ctx.writeln("")?;
				ctx.write_indent("}")
			},
			Tuple(tuple) => {
				ctx.write("(")?;
				for (i, expression) in tuple.expressions.iter().enumerate() {
					expression.pretty_print(ctx)?;
					if i != tuple.expressions.len() - 1 {
						ctx.write(", ")?;
					}
				}
				ctx.write(")")
			},
			TernaryExpression(ternary) => {
				ternary.condition.pretty_print(ctx)?;
				ctx.write(" ? ")?;
				ternary.true_branch.pretty_print(ctx)?;
				ctx.write(" : ")?;
				ternary.false_branch.pretty_print(ctx)
			},
			PostfixWithIndex(postfix) => {
				postfix.expression.pretty_print(ctx)?;
				ctx.write("[")?;
				postfix.index.pretty_print(ctx)?;
				ctx.write("]")
			},
			PostfixWithRange(postfix) => {
				postfix.expression.pretty_print(ctx)?;
				postfix.range.pretty_print(ctx)?;
				Ok(())
			},
			PostfixWithArgs(postfix) => {
				ctx.write(format!("{}", ctx.get_id(postfix.id)).as_str())?;
				ctx.write("(")?;
				for (i, expression) in postfix.argument_list.iter().enumerate() {
					expression.pretty_print(ctx)?;
					if i != postfix.argument_list.len() - 1 {
						ctx.write(", ")?;
					}
				}
				ctx.write(")")
			},
			PostfixWithId(postfix) => {
				postfix.expression.pretty_print(ctx)?;
				ctx.write(format!(".{}", ctx.get_id(postfix.id)).as_str())
			},
			UnaryOperatorExpression(unary) => {
				ctx.write(format!("{:?}", unary.code).as_str())?;
				unary.expression.pretty_print(ctx)
			},
			UnaryCastExpression(unary) => {
				ctx.write("(")?;
				unary.type_name.pretty_print(ctx)?;
				ctx.write(")")?;
				unary.expression.pretty_print(ctx)
			},
			BinaryExpression(binop) => {
				binop.lhs.pretty_print(ctx)?;
				ctx.write(format!(" {:?} ", binop.code).as_str())?;
				binop.rhs.pretty_print(ctx)
			},
		}
	}
}
