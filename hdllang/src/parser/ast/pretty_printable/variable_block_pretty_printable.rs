use crate::parser::ast::VariableBlock;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for VariableBlock {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.write_opt_newline("")?;
		ctx.write_indent("")?;
		for ty in &self.types {
			ty.pretty_print(ctx)?;
			ctx.write(" ")?;
		}
		ctx.increase_indent();
		ctx.writeln("{")?;
		for statement in &self.statements {
			ctx.write_indent("")?;
			statement.pretty_print(ctx)?;
		}
		ctx.decrease_indent();
		ctx.write_indent("}")?;
		ctx.writeln("")
	}
}
