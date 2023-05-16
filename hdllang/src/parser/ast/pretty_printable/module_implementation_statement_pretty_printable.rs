use crate::parser::ast::ModuleImplementationStatement;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for ModuleImplementationStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use ModuleImplementationStatement::*;
		match self {
			VariableDeclarationStatement { declaration, .. } => {
				ctx.after_brackets = false;
				ctx.write_opt_newline("")?;
				declaration.pretty_print(ctx)?;
				ctx.writeln(";")
			},
			VariableBlock { block, .. } => {
				block.pretty_print(ctx)
			},
			VariableDefinitionStatement { definition, .. } => {
				ctx.after_brackets = false;
				ctx.write_opt_newline("")?;
				definition.pretty_print(ctx)?;
				ctx.writeln(";")
			},
			AssignmentStatement {
				lhs,
				assignment_opcode,
				rhs,
				..
			} => {
				ctx.after_brackets = false;
				ctx.write_opt_newline("")?;
				lhs.pretty_print(ctx)?;
				ctx.write(format!(" {:?} ", assignment_opcode).as_str())?;
				rhs.pretty_print(ctx)?;
				ctx.writeln(";")
			},
			IfStatement {
				condition,
				if_statement,
				..
			} => {
				if !ctx.afer_else {
					ctx.afer_else = false;
					ctx.writeln("")?;
				}
				ctx.after_brackets = false;
				ctx.write_indent("if (")?;
				condition.pretty_print(ctx)?;
				ctx.write(") ")?;
				if_statement.pretty_print(ctx)
			},
			IfElseStatement {
				condition,
				if_statement,
				else_statement,
				..
			} => {
				if !ctx.afer_else {
					ctx.writeln("")?;
				}
				ctx.after_brackets = false;
				ctx.write_indent("if (")?;
				condition.pretty_print(ctx)?;
				ctx.write(") ")?;
				if_statement.pretty_print(ctx)?;
				ctx.write(" else ")?;
				ctx.after_brackets = false;
				ctx.afer_else = true;
				else_statement.pretty_print(ctx)
			},
			IterationStatement {
				id, range, statement, ..
			} => {
				ctx.after_brackets = false;
				ctx.write_opt_newline("")?;
				ctx.write("for (")?;
				ctx.write(format!("{}", &ctx.get_id(*id)).as_str())?;
				ctx.write(" in ")?;
				range.pretty_print(ctx)?;
				ctx.write(") ")?;
				statement.pretty_print(ctx)
			},
			InstantiationStatement {
				module_name: id1,
				instance_name: id2,
				port_bind,
				..
			} => {
				ctx.after_brackets = false;
				ctx.write_opt_newline("")?;
				ctx.write(format!("{} ", &ctx.get_id(*id1)).as_str())?;
				ctx.write(format!("{} ", &ctx.get_id(*id2)).as_str())?;
				ctx.increase_indent();
				ctx.writeln("{")?;
				for statement in port_bind {
					ctx.write_indent("")?;
					statement.pretty_print(ctx)?;
				}
				ctx.decrease_indent();
				ctx.write_indent("")?;
				ctx.writeln("}")
			},
			ModuleImplementationBlockStatement { statements, .. } => {
				if ctx.after_brackets {
					ctx.after_brackets = false;
					ctx.writeln("")?;
				}
				ctx.write_indent("{")?;
				ctx.after_brackets = true;
				ctx.increase_indent();
				for statement in statements {
					statement.pretty_print(ctx)?;
				}
				ctx.decrease_indent();
				ctx.write_opt_newline("}")?;
				ctx.after_brackets = true;
				Ok(())
				//ctx.writeln("")
			},
		}
	}
}
