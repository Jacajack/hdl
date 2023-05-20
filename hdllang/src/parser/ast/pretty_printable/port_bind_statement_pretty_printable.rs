use crate::parser::ast::PortBindStatement;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for PortBindStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use PortBindStatement::*;
		match self {
			OnlyId { id, .. } => ctx.writeln(format!("{},", ctx.get_id(*id)).as_str()),
			IdWithExpression { id, expression, .. } => {
				ctx.write(format!("{}: ", ctx.get_id(*id)).as_str())?;
				expression.pretty_print(ctx)?;
				ctx.writeln(",")
			},
			IdWithDeclaration { id, declaration, .. } => {
				ctx.write(format!("{}: ", ctx.get_id(*id)).as_str())?;
				declaration.pretty_print(ctx)?;
				ctx.writeln(",")
			},
		}
	}
}
