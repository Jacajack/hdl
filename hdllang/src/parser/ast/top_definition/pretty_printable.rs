use crate::parser::ast::{ModuleDeclaration, TopDefinition};
use crate::parser::pretty_printer::*;

use super::{ModuleImplementation, PackageDeclaration, UseStatement};

impl PrettyPrintable for ModuleDeclaration {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		for comment in self.metadata.iter() {
			ctx.writeln(format!("///{}", ctx.get_comment(*comment)).as_str())?;
		}
		ctx.write_indent("module ")?;
		ctx.write((&ctx.get_id(self.id)).to_string().as_str())?;
		ctx.writeln(" {")?;
		ctx.increase_indent();
		for statement in &self.statements {
			ctx.write_indent("")?;
			statement.pretty_print(ctx)?;
		}
		ctx.decrease_indent();
		ctx.write_opt_newline("}")?;
		ctx.writeln("")?;
		ctx.writeln("")?;
		Ok(())
	}
}

impl PrettyPrintable for ModuleImplementation {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		for comment in self.metadata.iter() {
			ctx.writeln(format!("///{}", ctx.get_comment(*comment)).as_str())?;
		}
		ctx.write_indent("impl ")?;
		ctx.write(format!("{} ", &ctx.get_id(self.id)).as_str())?;
		self.statement.pretty_print(ctx)?;
		ctx.writeln("")?;
		Ok(())
	}
}

impl PrettyPrintable for PackageDeclaration {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		for comment in self.metadata.iter() {
			ctx.writeln(format!("///{}", ctx.get_comment(*comment)).as_str())?;
		}
		ctx.write_indent("package ")?;
		self.path.pretty_print(ctx)?;
		ctx.writeln(";")
	}
}

impl PrettyPrintable for UseStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		for comment in self.metadata.iter() {
			ctx.writeln(format!("///{}", ctx.get_comment(*comment)).as_str())?;
		}
		ctx.write_indent("use ")?;
		self.path.pretty_print(ctx)?;
		ctx.writeln(";")
	}
}

impl PrettyPrintable for TopDefinition {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use TopDefinition::*;
		match self {
			ModuleDeclaration(declaration) => declaration.pretty_print(ctx),
			ModuleImplementation(implementation) => implementation.pretty_print(ctx),
			PackageDeclaration(package) => package.pretty_print(ctx),
			UseStatement(use_statement) => use_statement.pretty_print(ctx),
		}
	}
}
