use crate::parser::ast::{ModuleDeclarationStatement, VariableDeclarationStatement};
use crate::parser::pretty_printer::*;

use super::ModuleDeclarationVariableBlock;

impl PrettyPrintable for VariableDeclarationStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		self.type_declarator.pretty_print(ctx)?;
		for (i, direct) in self.direct_declarators.iter().enumerate() {
			direct.pretty_print(ctx)?;
			if i != self.direct_declarators.len() - 1 {
				ctx.write(", ")?;
			}
		}
		ctx.writeln(";")
	}
}
impl PrettyPrintable for ModuleDeclarationVariableBlock {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.write_opt_newline("")?;
		ctx.write_indent("")?;
		for ty in &self.types {
			ty.pretty_print(ctx)?;
			ctx.write(" ")?;
		}
		ctx.increase_indent();
		ctx.writeln("{")?;
		for statement in &self.statements {
			ctx.write_indent("")?;
			statement.pretty_print(ctx)?;
		}
		ctx.decrease_indent();
		ctx.write_indent("}")?;
		ctx.writeln("")
	}
}
impl PrettyPrintable for ModuleDeclarationStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement(declaration) => declaration.pretty_print(ctx),
			VariableBlock(block) => block.pretty_print(ctx),
		}
	}
}
