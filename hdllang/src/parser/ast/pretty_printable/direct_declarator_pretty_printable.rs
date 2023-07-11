use crate::parser::ast::DirectDeclarator;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for DirectDeclarator {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.write(ctx.get_id(self.name).to_string().as_str())?;
		for array in &self.array_declarators {
			ctx.write("[")?;
			array.pretty_print(ctx)?;
			ctx.write("]")?;
		}
		Ok(())
	}
}
