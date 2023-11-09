use crate::core::{NumericConstantTable, IdTable, IdTableKey,DiagnosticBuffer};
use crate::parser::ast::ModuleImplementation;
use crate::analyzer::ModuleDeclared;
use std::collections::HashMap;

/// Global shared context for semantic analysis
pub struct GlobalAnalyzerContext<'a> {
	pub id_table: &'a mut IdTable,
	pub nc_table: &'a NumericConstantTable,
	/// represents all declared modules
	pub modules_declared: HashMap<IdTableKey, ModuleDeclared>,
	/// represents all implemented generic modules
	pub generic_modules: HashMap<IdTableKey, &'a ModuleImplementation>,
	pub design: hirn::design::DesignHandle,

	pub diagnostic_buffer: DiagnosticBuffer,
}
