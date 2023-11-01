use crate::parser::ast::VariableDefinition;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for VariableDefinition {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		self.type_declarator.pretty_print(ctx)?;
		for i in 0..self.initializer_list.len() {
			self.initializer_list[i].pretty_print(ctx)?;
			if i != self.initializer_list.len() - 1 {
				ctx.write(", ")?;
			}
		}
		Ok(())
	}
}
