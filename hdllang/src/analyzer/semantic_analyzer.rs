use super::analyzer_pass::preamble::*;
use super::toplevel_pass::ToplevelPass;

pub struct SemanticAnalyzer<'source> {
	passes: Vec<Box<dyn AnalyzerPass<'source>>>,
	context: PassContext<'source>,
}

impl<'source> SemanticAnalyzer<'source> {
	pub fn new(id_table: &'source IdTable, comment_table: &'source CommentTable) -> Self {
		let context = PassContext {
			id_table,
			comment_table,
		};

		Self {
			passes: vec![Box::new(ToplevelPass {})],
			context,
		}
	}

	pub fn process(&mut self, ast: &'source Root) {
		for pass in &mut self.passes {
			pass.run(&self.context, ast);
		}
	}
}
