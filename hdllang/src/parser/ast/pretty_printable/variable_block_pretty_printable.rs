use crate::parser::ast::VariableBlock;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for VariableBlock {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		for statement in &self.statements {
			statement.pretty_print(ctx)?;
		}
		Ok(())
	}
}
