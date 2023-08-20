use crate::parser::ast::TypeQualifier;
use crate::parser::pretty_printer::*;

use super::{Comb, Sync};

impl PrettyPrintable for Comb{
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.write("comb(")?;
		for (i, expr) in self.expressions.iter().enumerate() {
			if i != 0 {
				ctx.write(", ")?;
			}
			expr.pretty_print(ctx)?;
		}
		ctx.write(")")
	}
}
impl PrettyPrintable for Sync{
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		ctx.write("sync(")?;
		for (i, expr) in self.expressions.iter().enumerate() {
			if i != 0 {
				ctx.write(", ")?;
			}
			expr.pretty_print(ctx)?;
		}
		ctx.write(")")
	}
}

impl PrettyPrintable for TypeQualifier {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use TypeQualifier::*;
		match self {
			Signed { .. } => ctx.write("signed"),
			Unsigned { .. } => ctx.write("unsigned"),
			Tristate { .. } => ctx.write("tristate"),
			Const { .. } => ctx.write("const"),
			Clock { .. } => ctx.write("clock"),
			Comb (comb) => comb.pretty_print(ctx),
			Sync (sync) => sync.pretty_print(ctx),
			Input { .. } => ctx.write("input"),
			Output { .. } => ctx.write("output"),
			Async { .. } => ctx.write("async"),
		}
	}
}
