use crate::parser::ast::TypeQualifier;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for TypeQualifier {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use TypeQualifier::*;
		match self {
			Signed { .. } => ctx.write("signed"),
			Unsigned { .. } => ctx.write("unsigned"),
			Tristate { .. } => ctx.write("tristate"),
			Const { .. } => ctx.write("const"),
			Clock { .. } => ctx.write("clock"),
			Comb { expression, .. } => {
				ctx.write("comb(")?;
				expression.pretty_print(ctx)?;
				ctx.write(")")
			},
			Sync { expressions, .. } => {
				ctx.write("sync(")?;
				for (i, expr) in expressions.iter().enumerate() {
					if i != 0 {
						ctx.write(", ")?;
					}
					expr.pretty_print(ctx)?;
				}
				ctx.write(")")
			},
			Input { .. } => ctx.write("input"),
			Output { .. } => ctx.write("output"),
			Async { .. } => ctx.write("async"),
		}
	}
}
