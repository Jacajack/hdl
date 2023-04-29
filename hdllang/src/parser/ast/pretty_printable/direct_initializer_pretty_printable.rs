use crate::parser::ast::DirectInitializer;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for DirectInitializer{
    fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
        use DirectInitializer::*;
        match self{
            DirectDeclarator { declarator, .. } => {
                declarator.pretty_print(ctx)
            },
            DirectDeclaratorWithInitializer { declarator, expression, .. } => {
                declarator.pretty_print(ctx)?;
                ctx.write(" = ")?;
                expression.pretty_print(ctx)
            }
        }
    }
}