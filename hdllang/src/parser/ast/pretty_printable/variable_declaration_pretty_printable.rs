use crate::parser::ast::VariableDeclaration;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for VariableDeclaration {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		self.type_declarator.pretty_print(ctx)?;
		// ctx.write(" ")?;
		for (i, direct) in self.direct_declarators.iter().enumerate() {
			direct.pretty_print(ctx)?;
			if i != self.direct_declarators.len() - 1 {
				ctx.write(", ")?;
			}
		}
		Ok(())
	}
}
