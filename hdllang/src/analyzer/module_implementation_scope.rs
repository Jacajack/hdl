use std::collections::HashMap;

use hirn::{SignalId, design::ModuleHandle};
use log::debug;

use crate::{lexer::{IdTableKey, IdTable}, SourceSpan, parser::ast::Scope};

use super::{Variable, VariableKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleImplementationScope {
	scopes: Vec<InternalScope>,
	api_ids: HashMap<usize, SignalId>,
	internal_ids: HashMap<usize, (usize, IdTableKey)>,
	variable_counter: usize,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InternalScope {
	variables: HashMap<IdTableKey, VariableDefined>,
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
		Self { scopes: vec![InternalScope::new(None)], api_ids: HashMap::new() , variable_counter: 0, internal_ids: HashMap::new()}
	}
	pub fn new_scope(&mut self, parent_scope: Option<usize>) -> usize {
		self.scopes.push(InternalScope::new(parent_scope));
		self.scopes.len() - 1
	}
	pub fn is_generic(&self) -> bool {
		if self.scopes.len() == 0 {
			return false;
		}
		 for var in self.scopes[0].variables.values() {
			if matches!(var.var.kind, VariableKind::Generic(_) ){
				true;
			}
		}
		false

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
	pub fn get_variable_in_scope(&self, scope_id: usize, key: &IdTableKey) -> Option<&VariableDefined> {
		let scope = &self.scopes[scope_id];
		scope.variables.get(key)
	}
	pub fn is_declared(&self, scope_key: usize, key: &IdTableKey) -> Option<SourceSpan> {
		self.scopes[scope_key].variables.get(key).map(|x| x.var.location)
	}
	pub fn get_api_id(&self, id: IdTableKey) -> Option<SignalId> {
		self.api_ids.get(&self.get_variable(0, &id).unwrap().id).cloned()
	}
	pub fn define_variable(&mut self, scope_id: usize, var: Variable) -> miette::Result<()> {
		let id = self.variable_counter;
		self.variable_counter += 1;
		let name = var.name.clone();
		let defined = VariableDefined { var, id };
		self.scopes[scope_id].variables.insert(name, defined);
		self.internal_ids.insert(id, (scope_id, name));
		Ok(())
	}
	pub fn declare_variable(&mut self, var: Variable, id_table: &IdTable, handle: &mut ModuleHandle) ->miette::Result<()>{
		let id = self.variable_counter;
		self.variable_counter += 1;
		let name = var.name.clone();
		self.internal_ids.insert(id, (0, name));
		self.api_ids.insert(id, var.register(id_table, &self, handle.scope().new_signal().unwrap())?);
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
}
impl Scope for ModuleImplementationScope {
    fn get_variable(&self, name: &IdTableKey) -> Option<Variable> {
		self.get_variable(0, name).map(|x| x.var.clone())
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableDefined {
	pub var: Variable,
	pub id: usize,
}

impl VariableDefined {
	pub fn is_clock(&self) -> bool {
		self.var.is_clock()
	}
}
