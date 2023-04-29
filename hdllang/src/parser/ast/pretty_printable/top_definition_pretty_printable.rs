use crate::parser::ast::TopDefinition;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for TopDefinition {
    fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
        use TopDefinition::*;
        match self {
            ModuleDeclaration { id, statements, .. } => {
                ctx.write_indent("module ")?;
                ctx.write(format!("{}", &ctx.get_id(*id)).as_str())?;
                ctx.writeln(" {")?;
                ctx.increase_indent();
                for statement in statements {
                    statement.pretty_print(ctx)?;
                }
                ctx.write_opt_newline("}")?;
                ctx.decrease_indent();
                Ok(())
            },
            ModuleImplementation { id, statement, .. } => {
                ctx.write_indent("impl ")?;
                ctx.write(format!("{} ", &ctx.get_id(*id)).as_str())?;
                statement.pretty_print(ctx)
            },
        }
    }
}