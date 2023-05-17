use crate::parser::ast::{ImportPath,Start,Modules};
use crate::parser::pretty_printer::*;

impl PrettyPrintable for ImportPath {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
        match &self.start {
            Start::Super => {
                ctx.write("super::")?;
            },
            Start::Root => {
                ctx.write("::")?;
            },
            Start::Local => {
                ctx.write("")?;
            },
        }
        for (i,path) in self.path.iter().enumerate() {
			ctx.write(format!("{}", &ctx.get_id(*path)).as_str())?;
            if i != self.path.len() - 1 {
                ctx.write("::")?;
            }
        }
        ctx.write("::")?;
        self.modules.pretty_print(ctx)?;
        Ok(())
        // match self.modules.len(){
        //     1 => ctx.write(format!("{}", &ctx.get_id(self.modules[0])).as_str()),
        //     _ => {
        //         ctx.write("{")?;
        //         for module in &self.modules {
        //             ctx.write(format!("{},", &ctx.get_id(*module)).as_str())?;
        //         }
        //         ctx.write("}")
        //     }
        // }
	}
}

impl PrettyPrintable for Modules {
    fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()> {
        use Modules::*;
        match self {
            All => {
                ctx.write("*")
            },
            Specific{modules} => {
                match modules.len() {
                    1 => ctx.write(format!("{}", &ctx.get_id(modules[0])).as_str()),
                    _ => {
                        ctx.write("{")?;
                        for module in modules {
                            ctx.write(format!("{},", &ctx.get_id(*module)).as_str())?;
                        }
                        ctx.write("}")
                    }
                }
            }
        }
    }
}