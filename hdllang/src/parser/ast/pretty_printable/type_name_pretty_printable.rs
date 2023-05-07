use crate::parser::ast::TypeName;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for TypeName {
    fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
        self.declarator.pretty_print(ctx)?;
        ctx.write(" ")?;
        for array in &self.array_declarators {
            array.pretty_print(ctx)?;
        }
        Ok(())
    }
}