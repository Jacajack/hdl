use std::{collections::HashMap, hash::Hash};

use hirn::design::{ModuleHandle, SignalId};
use log::*;

use crate::{
	lexer::{IdTable, IdTableKey},
	parser::ast::Scope,
	ProvidesCompilerDiagnostic, SourceSpan,
};

use super::{GlobalAnalyzerContext, SemanticError, Variable, VariableKind};
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct InternalVariableId {
	id: usize,
}

impl InternalVariableId {
	pub fn new(id: usize) -> Self {
		Self { id }
	}
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EvaluatedEntry {
	pub expression: crate::parser::ast::Expression,
	pub scope_id: usize,
}
impl EvaluatedEntry {
	pub fn new(expression: crate::parser::ast::Expression, scope_id: usize) -> Self {
		Self { expression, scope_id }
	}
}
use crate::analyzer::BusWidth;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleImplementationScope {
	pub widths: HashMap<SourceSpan, BusWidth>,
	pub evaluated_expressions: HashMap<SourceSpan, EvaluatedEntry>,
	pub enriched_constants: HashMap<SourceSpan, crate::core::NumericConstant>,
	scopes: Vec<InternalScope>,
	is_generic: bool,
	api_ids: HashMap<InternalVariableId, SignalId>,
	internal_ids: HashMap<InternalVariableId, (usize, IdTableKey)>,
	coupling_vars: HashMap<InternalVariableId, Vec<InternalVariableId>>,
	variable_counter: usize,
}
pub trait InternalScopeTrait {
	fn contains_key(&self, key: &IdTableKey) -> bool;
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InternalScope {
	variables: HashMap<IdTableKey, VariableDefined>,
	parent_scope: Option<usize>,
}
impl InternalScopeTrait for InternalScope {
	fn contains_key(&self, key: &IdTableKey) -> bool {
		self.variables.contains_key(key)
	}
}
impl InternalScope {
	pub fn new(parent_scope: Option<usize>) -> Self {
		Self {
			variables: HashMap::new(),
			parent_scope,
		}
	}
}
pub trait ScopeTrait {
	fn get_variable(&self, scope_id: usize, key: &IdTableKey) -> Option<&VariableDefined>;
	fn new_subscope(&mut self, parent_scope: Option<usize>) -> usize;
	fn mark_as_generic(&mut self);
	fn is_generic(&self) -> bool;
}
impl ModuleImplementationScope {
	pub fn display_interface(&self, id_table: &IdTable) -> String {
		let mut s = String::new();
		let scope = self.scopes.first().unwrap();
		for (name, var) in &scope.variables {
			s += format!("Variable {}: {:?}\n", id_table.get_value(name), var.var.kind).as_str();
		}
		s += format!("dupa").as_str();

		s
	}
	pub fn get_interface_len(&self) -> usize {
		return self.scopes.first().unwrap().variables.len();
	}
	pub fn get_var(&self, scope_id: usize, name: &IdTableKey) -> Result<&VariableDefined, SemanticError> {
		let scope = &self.scopes[scope_id];
		if let Some(variable) = scope.variables.get(name) {
			Ok(variable)
		}
		else {
			if let Some(parent_scope) = scope.parent_scope {
				self.get_var(parent_scope, name)
			}
			else {
				Err(SemanticError::VariableNotDeclared)
			}
		}
	}
	pub fn transorm_to_generic(&mut self) {
		debug!("Transforming scope to generic");
		for scope in self.scopes.iter_mut() {
			for var in scope.variables.values_mut() {
				match &mut var.var.kind {
					VariableKind::Signal(sig) => {
						sig.dimensions.iter_mut().for_each(|dim| {
							debug!("Transforming dimension {:?} to generic", dim);
							dim.to_generic();
							debug!("Transformed dimension {:?} to generic", dim);
						});
						use crate::analyzer::SignalType::*;
						match &mut sig.signal_type {
							Bus(bus) => match &mut bus.width {
								Some(w) => w.to_generic(),
								None => (),
							},
							Wire(_) => (),
							Auto(_) => unreachable!(),
						}
					},
					VariableKind::Generic(gen) => {
						gen.dimensions.iter_mut().for_each(|dim| {
							debug!("Transforming dimension {:?} to generic", dim);
							dim.to_generic();
							debug!("Transformed dimension {:?} to generic", dim);
						});
						match &mut gen.value {
							// unreachable!(),
							Some(val) => {
								debug!("Transforming value {:?} to generic", val);
								val.to_generic();
								debug!("Transformed value {:?} to generic", val);
							},
							None => (),
						}
					},
					VariableKind::ModuleInstance(_) => {
						unreachable!("Module instantion can't be declared in module implementation scope")
					},
				}
			}
		}
	}
	pub fn new() -> Self {
		Self {
			widths: HashMap::new(),
			evaluated_expressions: HashMap::new(),
			scopes: vec![InternalScope::new(None)],
			api_ids: HashMap::new(),
			variable_counter: 0,
			internal_ids: HashMap::new(),
			is_generic: false,
			coupling_vars: HashMap::new(),
			enriched_constants: HashMap::new(),
		}
	}
	pub fn add_coupling(&mut self, from: IdTableKey, to: IdTableKey, scope_id: usize) {
		let from_id = self.get_variable(scope_id, &from).unwrap().id;
		let to_id = self.get_variable(scope_id, &to).unwrap().id;
		self.coupling_vars
			.entry(from_id)
			.and_modify(|x| x.push(to_id))
			.or_insert(vec![to_id]);
	}
	pub fn new_scope(&mut self, parent_scope: Option<usize>) -> usize {
		self.scopes.push(InternalScope::new(parent_scope));
		self.scopes.len() - 1
	}
	pub fn mark_as_generic(&mut self) {
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
	pub fn redeclare_variable(&mut self, var: VariableDefined) {
		let (scope_id, name) = self.internal_ids.get(&var.id).unwrap();
		info!("Redeclared variable {:?} in scope {}", var, scope_id);
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
	pub fn declare_variable(
		&mut self,
		var: Variable,
		nc_table: &crate::lexer::NumericConstantTable,
		id_table: &IdTable,
		handle: &mut ModuleHandle,
	) -> miette::Result<()> {
		match self.is_declared(0, &var.name) {
			Some(location) => {
				return Err(miette::Report::new(
					SemanticError::DuplicateVariableDeclaration
						.to_diagnostic_builder()
						.label(var.location, "Variable with this name already exists")
						.label(location, "Variable with this name already declared here")
						.build(),
				))
			},
			None => (),
		}
		let id = InternalVariableId::new(self.variable_counter);
		self.variable_counter += 1;
		let name = var.name.clone();
		self.internal_ids.insert(id, (0, name));
		self.api_ids.insert(
			id,
			var.register(
				nc_table,
				id_table,
				0,
				&self,
				handle
					.scope()
					.new_signal(id_table.get_by_key(&name).unwrap().as_str())
					.unwrap(),
			)?,
		);
		match &var.kind {
			VariableKind::Generic(_) => handle
				.expose(
					self.api_ids.get(&id).unwrap().clone(),
					hirn::design::SignalDirection::Input,
				)
				.unwrap(),
			VariableKind::Signal(sig) => match &sig.direction {
				super::Direction::Input(_) => handle
					.expose(
						self.api_ids.get(&id).unwrap().clone(),
						hirn::design::SignalDirection::Input,
					)
					.unwrap(),
				super::Direction::Output(_) => handle
					.expose(
						self.api_ids.get(&id).unwrap().clone(),
						hirn::design::SignalDirection::Output,
					)
					.unwrap(),
				_ => unreachable!("Only input and output signals can be declared in module implementation scope"),
			},
			VariableKind::ModuleInstance(_) => {
				unreachable!("Module instantion can't be declared in module implementation scope")
			},
		}
		let defined = VariableDefined { var, id };
		self.scopes[0].variables.insert(name, defined);
		Ok(())
	}
	pub fn second_pass(&self, _: &GlobalAnalyzerContext) -> miette::Result<()> {
		for scope in &self.scopes {
			for var in scope.variables.values() {
				match &var.var.kind {
					VariableKind::Signal(sig) => {
						if !sig.is_sensititivity_specified() {
							return Err(miette::Report::new(
								SemanticError::MissingSensitivityQualifier
									.to_diagnostic_builder()
									.label(
										var.var.location,
										"Signal must be either const, clock, comb, sync or async",
									)
									.build(),
							));
						}
						if !sig.is_signedness_specified() {
							return Err(miette::Report::new(
								SemanticError::MissingSignednessQualifier
									.to_diagnostic_builder()
									.label(var.var.location, "Bus signal must be either signed or unsigned")
									.build(),
							));
						}
					},
					VariableKind::Generic(_) => (),
					VariableKind::ModuleInstance(_) => (),
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
