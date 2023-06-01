use crate::parser::ast::TypeSpecifier;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for TypeSpecifier {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use TypeSpecifier::*;
		match self {
			Auto { .. } => ctx.write("auto"),
			Int { .. } => ctx.write("int"),
			Wire { .. } => ctx.write("wire"),
			Bool { .. } => ctx.write("bool"),
			Bus { width, .. } => {
				ctx.write("bus")?;
				ctx.write("<")?;
				width.pretty_print(ctx)?;
				ctx.write(">")
			},
		}
	}
}
