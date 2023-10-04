use std::collections::HashMap;

use hirn::{SignalId, design::ModuleHandle};
use log::debug;

use crate::{lexer::{IdTableKey, IdTable}, SourceSpan, parser::ast::Scope, ProvidesCompilerDiagnostic};

use super::{Variable, VariableKind, GlobalAnalyzerContext, SemanticError};
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct InternalVariableId {
	id: usize,
}

impl InternalVariableId {
	pub fn new(id: usize) -> Self {
		Self { id }
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleImplementationScope {
	scopes: Vec<InternalScope>,
	is_generic: bool,
	api_ids: HashMap<InternalVariableId, SignalId>,
	internal_ids: HashMap<InternalVariableId, (usize, IdTableKey)>,
	variable_counter: usize,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InternalScope {
	pub variables: HashMap<IdTableKey, VariableDefined>,
	parent_scope: Option<usize>,
}
impl InternalScope {
	pub fn new(parent_scope: Option<usize>) -> Self {
		Self {
			variables: HashMap::new(),
			parent_scope,
		}
	}
	pub fn contains_key(&self, key: &IdTableKey) -> bool {
		self.variables.contains_key(key)
	}
}
impl ModuleImplementationScope {
	pub fn new() -> Self {
		Self { scopes: vec![InternalScope::new(None)], api_ids: HashMap::new() , variable_counter: 0, internal_ids: HashMap::new(), is_generic: false}
	}
	pub fn new_scope(&mut self, parent_scope: Option<usize>) -> usize {
		self.scopes.push(InternalScope::new(parent_scope));
		self.scopes.len() - 1
	}
	pub fn mark_as_generic(&mut self){
		self.is_generic = true;
	}
	pub fn is_generic(&self) -> bool {
		self.is_generic
	}
	pub fn get_scope(&self, scope_id: usize) -> &InternalScope {
		&self.scopes[scope_id]
	}
	pub fn get_variable(&self, scope_id: usize, key: &IdTableKey) -> Option<&VariableDefined> {
		let scope = &self.scopes[scope_id];
		if let Some(variable) = scope.variables.get(key) {
			Some(variable)
		}
		else {
			if let Some(parent_scope) = scope.parent_scope {
				self.get_variable(parent_scope, key)
			}
			else {
				None
			}
		}
	}
	pub fn redeclare_variable(&mut self, var: VariableDefined){
		let (scope_id, name) = self.internal_ids.get(&var.id).unwrap();
		debug!("Redeclared variable {:?} in scope {}", var, scope_id);
		self.scopes[*scope_id].variables.insert(*name, var);
	}
	pub fn get_variable_in_scope(&self, scope_id: usize, key: &IdTableKey) -> Option<&VariableDefined> {
		let scope = &self.scopes[scope_id];
		scope.variables.get(key)
	}
	pub fn is_declared(&self, scope_key: usize, key: &IdTableKey) -> Option<SourceSpan> {
		self.scopes[scope_key].variables.get(key).map(|x| x.var.location)
	}
	pub fn insert_api_id(&mut self, id: InternalVariableId, api_id: SignalId) {
		self.api_ids.insert(id, api_id);
	}
	pub fn get_api_id(&self, scope_id: usize, key: &IdTableKey) -> Option<SignalId> {
		let scope = &self.scopes[scope_id];
		if let Some(variable) = scope.variables.get(key) {
			Some(self.api_ids.get(&variable.id).unwrap().clone())
		}
		else {
			if let Some(parent_scope) = scope.parent_scope {
				self.get_api_id(parent_scope, key)
			}
			else {
				None
			}
		}
	}
	pub fn define_variable(&mut self, scope_id: usize, var: Variable) -> miette::Result<()> {
		let id = InternalVariableId::new(self.variable_counter);
		self.variable_counter += 1;
		let name = var.name.clone();
		let defined = VariableDefined { var, id };
		self.scopes[scope_id].variables.insert(name, defined);
		self.internal_ids.insert(id, (scope_id, name));
		Ok(())
	}
	pub fn declare_variable(&mut self, var: Variable, id_table: &IdTable, handle: &mut ModuleHandle) ->miette::Result<()>{
		let id = InternalVariableId::new(self.variable_counter);
		self.variable_counter += 1;
		let name = var.name.clone();
		self.internal_ids.insert(id, (0, name));
		self.api_ids.insert(id, var.register(id_table, 0, &self, handle.scope().new_signal().unwrap())?);
		match &var.kind{
			VariableKind::Generic(_) => handle.expose(self.api_ids.get(&id).unwrap().clone(), hirn::design::SignalDirection::Input).unwrap(),
			VariableKind::Signal(sig) => {
				match &sig.direction{
        		super::Direction::Input(_) => handle.expose(self.api_ids.get(&id).unwrap().clone(), hirn::design::SignalDirection::Input).unwrap(),
        		super::Direction::Output(_) =>handle.expose(self.api_ids.get(&id).unwrap().clone(), hirn::design::SignalDirection::Output).unwrap(),
        		_ => unreachable!("Only input and output signals can be declared in module implementation scope"),
    		}
			}
		}
		let defined = VariableDefined { var, id };
		self.scopes[0].variables.insert(name, defined);
		Ok(())
	}
	pub fn second_pass(&self, ctx: &GlobalAnalyzerContext) -> miette::Result<()>{
		for scope in &self.scopes {
			for var in scope.variables.values() {
				match &var.var.kind{
					VariableKind::Signal(sig) => {
						if ! sig.is_sensititivity_specified(){
							return Err(miette::Report::new(
								SemanticError::MissingSensitivityQualifier
									.to_diagnostic_builder()
									.label(
										var.var.location,
										"Signal must be either const, clock, comb, sync or async"
									)
									.build(),
							));
						}
						if ! sig.is_signedness_specified() {
							return Err(miette::Report::new(
								SemanticError::MissingSignednessQualifier
									.to_diagnostic_builder()
									.label(
										var.var.location,
										"Bus signal must be either signed or unsigned"
									)
									.build(),
							));
						}
					},
					VariableKind::Generic(_) => (),
				}
			}
		}
		Ok(())
	}
}
impl Scope for ModuleImplementationScope {
    fn get_variable(&self, name: &IdTableKey) -> Option<Variable> {
		self.get_variable(0, name).map(|x| x.var.clone())
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableDefined {
	pub var: Variable,
	pub id: InternalVariableId,
}

impl VariableDefined {
	pub fn is_clock(&self) -> bool {
		self.var.is_clock()
	}
}
