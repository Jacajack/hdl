use crate::parser::ast::VariableBlockStatement;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for VariableBlockStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use VariableBlockStatement::*;
		match self {
			VariableDeclarationStatement { declaration, .. } => {
				declaration.pretty_print(ctx)?;
				ctx.writeln(";")
			},
			VariableBlock { block, .. } => {
				ctx.increase_indent();
				ctx.writeln("{")?;
				block.pretty_print(ctx)?;
				ctx.decrease_indent();
				ctx.writeln("}")
			},
		}
	}
}
