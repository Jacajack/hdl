use crate::parser::ast::VariableDefinition;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for VariableDefinition {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		self.type_declarator.pretty_print(ctx)?;
		for direct in &self.initializer_list {
			direct.pretty_print(ctx)?;
		}
		Ok(())
	}
}
