use crate::parser::ast::ModuleDeclarationStatement;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for ModuleDeclarationStatement {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement {  type_declarator, direct_declarators, ..  } => {
				type_declarator.pretty_print(ctx)?;
				for (i, direct) in direct_declarators.iter().enumerate() {
					direct.pretty_print(ctx)?;
					if i != direct_declarators.len() - 1 {
						ctx.write(", ")?;
					}
				}
				ctx.writeln(";")
			},
			VariableBlock {  types, statements, ..  } => {
				ctx.write_opt_newline("")?;
				ctx.write_indent("")?;
				for ty in types {
					ty.pretty_print(ctx)?;
					ctx.write(" ")?;
				}
				ctx.increase_indent();
				ctx.writeln("{")?;
				for statement in statements {
					ctx.write_indent("")?;
					statement.pretty_print(ctx)?;
				}
				ctx.decrease_indent();
				ctx.write_indent("}")?;
				ctx.writeln("")
			},
		}
	}
}
