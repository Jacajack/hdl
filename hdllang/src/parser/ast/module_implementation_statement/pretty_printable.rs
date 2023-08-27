use crate::parser::ast::ModuleImplementationStatement;
use crate::parser::pretty_printer::*;

use super::{
	AssignmentStatement, IfElseStatement, InstantiationStatement, IterationStatement,
	ModuleImplementationBlockStatement,
};

impl PrettyPrintable for AssignmentStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.after_brackets = false;
		ctx.write_opt_newline("")?;
		self.lhs.pretty_print(ctx)?;
		ctx.write(format!(" {:?} ", self.assignment_opcode).as_str())?;
		self.rhs.pretty_print(ctx)?;
		ctx.writeln(";")
	}
}

impl PrettyPrintable for IfElseStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		if !ctx.afer_else {
			ctx.writeln("")?;
		}
		ctx.after_brackets = false;
		ctx.write_indent("if (")?;
		self.condition.pretty_print(ctx)?;
		ctx.write(") ")?;
		self.if_statement.pretty_print(ctx)?;
		if let Some(stmt) = &self.else_statement {
			ctx.write(" else ")?;
			ctx.after_brackets = false;
			ctx.afer_else = true;
			stmt.pretty_print(ctx)?;
		}
		Ok(())
	}
}

impl PrettyPrintable for IterationStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.after_brackets = false;
		ctx.write_opt_newline("")?;
		ctx.write("for (")?;
		ctx.write((&ctx.get_id(self.id)).to_string().as_str())?;
		ctx.write(" in ")?;
		self.range.pretty_print(ctx)?;
		ctx.write(") ")?;
		self.statement.pretty_print(ctx)
	}
}

impl PrettyPrintable for InstantiationStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.after_brackets = false;
		ctx.write_opt_newline("")?;
		self.module_name.pretty_print(ctx)?;
		ctx.write(format!(" {} ", &ctx.get_id(self.instance_name)).as_str())?;
		ctx.increase_indent();
		ctx.writeln("{")?;
		for statement in &self.port_bind {
			ctx.write_indent("")?;
			statement.pretty_print(ctx)?;
		}
		ctx.decrease_indent();
		ctx.write_indent("")?;
		ctx.writeln("}")
	}
}

impl PrettyPrintable for ModuleImplementationBlockStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		if ctx.after_brackets {
			ctx.after_brackets = false;
			ctx.writeln("")?;
		}
		ctx.write_indent("{")?;
		ctx.after_brackets = true;
		ctx.increase_indent();
		for statement in &self.statements {
			statement.pretty_print(ctx)?;
		}
		ctx.decrease_indent();
		ctx.write_opt_newline("}")?;
		ctx.after_brackets = true;
		Ok(())
	}
}
impl PrettyPrintable for ModuleImplementationStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use ModuleImplementationStatement::*;
		match self {
			VariableBlock(block) => block.pretty_print(ctx),
			VariableDefinition(definition) => {
				ctx.after_brackets = false;
				ctx.write_opt_newline("")?;
				definition.pretty_print(ctx)?;
				ctx.writeln(";")
			},
			AssignmentStatement(assignment_statement) => assignment_statement.pretty_print(ctx),
			IfElseStatement(if_else) => if_else.pretty_print(ctx),
			IterationStatement(iteration) => iteration.pretty_print(ctx),
			InstantiationStatement(instantation) => instantation.pretty_print(ctx),
			ModuleImplementationBlockStatement(block) => block.pretty_print(ctx),
		}
	}
}
