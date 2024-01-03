use crate::analyzer::ModuleDeclared;
use crate::core::{DiagnosticBuffer, IdTable, IdTableKey, NumericConstantTable};
use crate::parser::ast::ModuleImplementation;
use std::collections::HashMap;

/// Global shared context for semantic analysis
pub struct GlobalAnalyzerContext<'a> {
	pub id_table: IdTable,
	pub nc_table: NumericConstantTable,
	pub comment_table: crate::lexer::CommentTable,
	/// represents all declared modules
	pub modules_declared: HashMap<IdTableKey, ModuleDeclared>,
	/// represents all implemented generic modules
	pub generic_modules: HashMap<IdTableKey, &'a ModuleImplementation>,
	pub design: hirn::design::DesignHandle,

	pub diagnostic_buffer: DiagnosticBuffer,
}
