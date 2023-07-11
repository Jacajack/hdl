use crate::parser::ast::{MatchExpressionAntecendent, MatchExpressionStatement};
use crate::parser::pretty_printer::*;

impl PrettyPrintable for MatchExpressionStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.write_indent("")?;
		self.antecedent.pretty_print(ctx)?;
		ctx.write(" => ")?;
		self.expression.pretty_print(ctx)?;
		ctx.writeln(",")
	}
}
impl PrettyPrintable for MatchExpressionAntecendent {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use MatchExpressionAntecendent::*;
		match self {
			Expression { expressions, .. } => {
				for (i, expression) in expressions.iter().enumerate() {
					if i != 0 {
						ctx.write(", ")?;
					}
					expression.pretty_print(ctx)?;
				}
				Ok(())
			},
			Default { .. } => ctx.write("default"),
		}
	}
}
