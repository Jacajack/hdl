use crate::parser::ast::TopDefinition;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for TopDefinition {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use TopDefinition::*;
		match self {
			ModuleDeclaration {
				metadata,
				id,
				statements,
				..
			} => {
				for comment in metadata.iter() {
					ctx.writeln(format!("///{}", ctx.get_comment(*comment)).as_str())?;
				}
				ctx.write_indent("module ")?;
				ctx.write(format!("{}", &ctx.get_id(*id)).as_str())?;
				ctx.writeln(" {")?;
				ctx.increase_indent();
				for statement in statements {
					ctx.write_indent("")?;
					statement.pretty_print(ctx)?;
				}
				ctx.decrease_indent();
				ctx.write_opt_newline("}")?;
				ctx.writeln("")?;
				ctx.writeln("")?;
				Ok(())
			},
			ModuleImplementation {
				metadata,
				id,
				statement,
				..
			} => {
				for comment in metadata.iter() {
					ctx.writeln(format!("///{}", ctx.get_comment(*comment)).as_str())?;
				}
				ctx.write_indent("impl ")?;
				ctx.write(format!("{} ", &ctx.get_id(*id)).as_str())?;
				statement.pretty_print(ctx)?;
				ctx.writeln("")?;
				Ok(())
			},
		}
	}
}
