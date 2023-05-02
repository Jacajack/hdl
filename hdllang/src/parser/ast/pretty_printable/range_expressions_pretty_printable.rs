use crate::parser::ast::RangeExpression;
use crate::parser::pretty_printer::*;

impl PrettyPrintable for RangeExpression{
    fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
        ctx.write("[")?;
		self.lhs.pretty_print(ctx)?;
		ctx.write(format!(" {:?} ", self.code).as_str())?;
		self.rhs.pretty_print(ctx)?;
		ctx.write("]")
    }
}