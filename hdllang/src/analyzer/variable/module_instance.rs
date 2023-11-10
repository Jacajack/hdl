use crate::ProvidesCompilerDiagnostic;
use crate::analyzer::SemanticError;
use crate::core::{IdTableKey, SourceSpan, CompilerDiagnosticBuilder};
use crate::analyzer::variable::InternalVariableId;
use std::collections::HashMap;
#[derive(Clone, Debug, PartialEq, Eq)]
/// To be deleted and replaced by unique module instance
pub struct RegisterInstance {
	pub name: IdTableKey,
	pub location: SourceSpan,
	pub next: InternalVariableId,
	pub clk: InternalVariableId,
	pub nreset: InternalVariableId,
	pub data: InternalVariableId,
	pub enable: InternalVariableId,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonRegister {
	pub interface: HashMap<IdTableKey, InternalVariableId>,
	pub clocks: Vec<InternalVariableId>,
}

impl NonRegister {
	pub fn new() -> Self {
		Self {
			interface: HashMap::new(),
			clocks: Vec::new(),
		}
	}
	pub fn add_variable(&mut self, name: IdTableKey, var: InternalVariableId) -> Result<(), CompilerDiagnosticBuilder> {
		match self.interface.insert(name, var) {
			Some(_) => Err(SemanticError::DuplicateVariableDeclaration.to_diagnostic_builder()),
			None => Ok(()),
		}
	}
	pub fn add_clock(&mut self, name: IdTableKey, var: InternalVariableId) -> Result<(), CompilerDiagnosticBuilder> {
		self.clocks.push(var);
		self.add_variable(name, var)
	}
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ModuleInstanceKind {
	Module(NonRegister),
	Register(RegisterInstance),
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleInstance {
	pub module_name: IdTableKey,
	pub location: SourceSpan,
	pub kind: ModuleInstanceKind,
}
impl ModuleInstance {
	pub fn new(module_name: IdTableKey, location: SourceSpan) -> Self {
		Self {
			module_name,
			location,
			kind: ModuleInstanceKind::Module(NonRegister::new()),
		}
	}
}
