use crate::parser::ast::Root;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for Root{
    fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
        for definition in &self.definitions{
            definition.pretty_print(ctx)?;
        }
        Ok(())
    }
}