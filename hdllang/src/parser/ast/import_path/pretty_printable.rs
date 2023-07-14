use crate::parser::ast::{ImportPath, Modules, Start};
use crate::parser::pretty_printer::*;

impl PrettyPrintable for ImportPath {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		match &self.start {
			Start::Super { number } => {
				for _ in 0..*number {
					ctx.write("super::")?;
				}
			},
			Start::Root => {
				ctx.write("root::")?;
			},
			Start::Local => {
				ctx.write("")?;
			},
		}
		for (i, path) in self.path.iter().enumerate() {
			ctx.write((&ctx.get_id(*path)).to_string().as_str())?;
			if i != self.path.len() - 1 {
				ctx.write("::")?;
			}
		}
		if !self.path.is_empty(){
			ctx.write("::")?;
		}
		self.modules.pretty_print(ctx)?;
		Ok(())
	}
}

impl PrettyPrintable for Modules {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
		use Modules::*;
		match self {
			All => ctx.write("*"),
			Specific { modules } => match modules.len() {
				1 => ctx.write((&ctx.get_id(modules[0])).to_string().as_str()),
				_ => {
					ctx.write("{")?;
					for module in modules {
						ctx.write(format!("{}, ", &ctx.get_id(*module)).as_str())?;
					}
					ctx.write("}")
				},
			},
		}
	}
}
