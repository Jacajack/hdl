use crate::core::DiagnosticBuffer;

use super::analyzer_pass::preamble::*;
use super::toplevel_pass::ToplevelPass;

pub struct SemanticAnalyzer<'source> {
	passes: Vec<Box<dyn AnalyzerPass<'source>>>,
	context: PassContext<'source>,
}

impl<'source> SemanticAnalyzer<'source> {
	pub fn new(id_table: &'source IdTable, comment_table: &'source CommentTable, diagnostics: &'source mut DiagnosticBuffer<'source>) -> Self {
		let context = PassContext {
			id_table,
			comment_table,
			diagnostics
		};

		Self {
			passes: vec![Box::new(ToplevelPass {})],
			context,
		}
	}

	pub fn process(&mut self, ast: &'source Root) -> miette::Result<()>{
		println!("Running semantic analyzer");
		for pass in &mut self.passes {
			pass.run(&mut self.context, ast)?;
		}
		println!("Semantic analyzer finished");
		println!("{}", self.context.diagnostics);
		Ok(())
	}
}
