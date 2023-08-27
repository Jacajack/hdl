use std::collections::HashMap;

use num_bigint::BigInt;

use crate::{lexer::IdTableKey, parser::ast::TypeSpecifier, SourceSpan};

use super::CombinedQualifiers;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleImplementationScope{
	scopes: Vec<Scope>
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope{
	variables: HashMap<IdTableKey, VariableDefined>,
	parent_scope: Option<usize>
}
impl Scope{
	pub fn new(parent_scope: Option<usize>) -> Self{
		Self{
			variables: HashMap::new(),
			parent_scope
		}
	}
	pub fn contains_key(&self, key: &IdTableKey) -> bool{
		self.variables.contains_key(key)
	}
}
impl ModuleImplementationScope{
	pub fn new() -> Self{
		Self{
			scopes: Vec::new()
		}
	}
	pub fn new_scope(&mut self, parent_scope: Option<usize>) -> usize{
		self.scopes.push(Scope::new(parent_scope));
		self.scopes.len() - 1
	}
	pub fn get_scope(&self, scope_id: usize) -> &Scope{
		&self.scopes[scope_id]
	}
	pub fn get_variable(&self, scope_id: usize, key: &IdTableKey) -> Option<&VariableDefined>{
		let scope = &self.scopes[scope_id];
		if let Some(variable) = scope.variables.get(key){
			Some(variable)
		} else {
			if let Some(parent_scope) = scope.parent_scope{
				self.get_variable(parent_scope, key)
			}else{
				None
			}
		}
	}
	pub fn get_variable_in_scope(&self, scope_id: usize, key: &IdTableKey) -> Option<&VariableDefined>{
		let scope = &self.scopes[scope_id];
		scope.variables.get(key)
	}
	pub fn is_declared(&self, scope_key: usize, key: &IdTableKey) -> Option<SourceSpan>{
		self.scopes[scope_key].variables.get(key).map(|x| x.location)
	}

}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableDefined{
	pub qualifiers: CombinedQualifiers,
	pub specifier: TypeSpecifier,
	pub bus_width: Option<BigInt>,
	pub dimensions: Vec<BigInt>,
	pub location: SourceSpan,
}