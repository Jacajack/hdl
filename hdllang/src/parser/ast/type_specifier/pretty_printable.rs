use crate::parser::ast::TypeSpecifier;
use crate::parser::pretty_printer::*;

use super::Bus;
impl PrettyPrintable for Bus{
    fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
        ctx.write("bus")?;
		ctx.write("<")?;
		self.width.pretty_print(ctx)?;
		ctx.write(">")
    }
}
impl PrettyPrintable for TypeSpecifier {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use TypeSpecifier::*;
		match self {
			Auto { .. } => ctx.write("auto"),
			Int { .. } => ctx.write("int"),
			Wire { .. } => ctx.write("wire"),
			Bool { .. } => ctx.write("bool"),
			Bus (bus) => bus.pretty_print(ctx),
		}
	}
}
