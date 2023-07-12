use crate::parser::ast::DirectInitializer;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for DirectInitializer {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		self.declarator.pretty_print(ctx)?;
		if self.expression.is_some() {
			ctx.write(" = ")?;
			self.expression.as_ref().unwrap().pretty_print(ctx)?;
		}
		Ok(())
	}
}
