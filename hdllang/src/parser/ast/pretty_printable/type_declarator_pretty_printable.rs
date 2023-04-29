use crate::parser::ast::TypeDeclarator;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for TypeDeclarator {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		for qualifier in &self.qualifiers {
			qualifier.pretty_print(ctx)?;
			ctx.write(" ")?;
		}
		self.specifier.pretty_print(ctx)?;
		ctx.write(" ")?;
		Ok(())
	}
}
