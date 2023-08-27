pub mod preamble {
	pub use super::AnalyzerPass;
	pub use super::PassContext;
	pub use crate::core::CommentTable;
	pub use crate::core::IdTable;
	pub use crate::parser::ast::Root;
}

use crate::core::DiagnosticBuffer;
use preamble::*;

pub struct PassContext<'source> {
	pub id_table: &'source IdTable,
	pub comment_table: &'source CommentTable,
	pub diagnostics: &'source mut DiagnosticBuffer<'source>,
	// namespace_defs: &'source mut NamespaceTable,
	// module_defs: &'source mut ModuleDefTable,
	// module_impls: &'source mut ModuleImplTable,
}

pub trait AnalyzerPass<'source> {
	fn run(&mut self, ctx: &mut PassContext<'source>, ast: &'source Root) -> miette::Result<()>;
}
