use log::info;

use super::analyzer_pass::preamble::*;

pub struct ToplevelPass {}

impl<'source> AnalyzerPass<'source> for ToplevelPass {
	fn run(&mut self, _ctx: &mut PassContext<'source>, ast: &'source Root) -> miette::Result<()> {
		info!("Running toplevel pass");
		for _def in &ast.definitions {
			//use crate::parser::ast::TopDefinition::*;
			//match def {
			//	ModuleDeclaration { id, .. } => {
			//		debug!("Found module def for {:?}", ctx.id_table.get_by_key(id).unwrap())
			//	}, // FIXME
			//	ModuleImplementation { id, .. } => {
			//		debug!("Found module impl for {:?}", ctx.id_table.get_by_key(id).unwrap())
			//	},
			//	PackageDeclaration { .. } => todo!(),
			//	UseStatement { .. } => todo!(), // FIXME
			//}
		}
		Ok(())
	}
}
