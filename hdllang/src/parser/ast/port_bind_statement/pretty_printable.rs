use crate::parser::ast::PortBindStatement;
use crate::parser::pretty_printer::*;

use super::{IdWithDeclaration, IdWithExpression, OnlyId};

impl PrettyPrintable for IdWithDeclaration {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.write(format!("{}: ", ctx.get_id(self.id)).as_str())?;
		self.declaration.pretty_print(ctx)?;
		ctx.writeln(",")
	}
}

impl PrettyPrintable for IdWithExpression {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.write(format!("{}: ", ctx.get_id(self.id)).as_str())?;
		self.expression.pretty_print(ctx)?;
		ctx.writeln(",")
	}
}

impl PrettyPrintable for OnlyId {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.writeln(format!("{},", ctx.get_id(self.id)).as_str())
	}
}

impl PrettyPrintable for PortBindStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.pretty_print(ctx),
			IdWithExpression(id_with_expression) => id_with_expression.pretty_print(ctx),
			IdWithDeclaration(id_with_declaration) => id_with_declaration.pretty_print(ctx),
		}
	}
}
