use crate::parser::ast::ModuleDeclarationStatement;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for ModuleDeclarationStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement { declaration, .. } => {
				declaration.pretty_print(ctx)?;
				ctx.writeln(";")
			},
			VariableBlock { block, .. } => {
				// ctx.increase_indent();
				// ctx.writeln("{")?;
				block.pretty_print(ctx)?;
				Ok(())
				// ctx.decrease_indent();
				// ctx.writeln("}")
			},
		}
	}
}
