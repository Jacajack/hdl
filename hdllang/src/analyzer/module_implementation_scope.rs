use std::{collections::HashMap, hash::Hash};

use bimap::BiHashMap;
use hirn::design::{ModuleHandle, ScopeHandle, SignalId};
use log::*;

use crate::{
	lexer::{IdTable, IdTableKey},
	ProvidesCompilerDiagnostic, SourceSpan,
};

use super::{
	AdditionalContext, DependencyGraph, GlobalAnalyzerContext, SemanticError, SignalSensitivity, Variable, VariableKind,
};
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash, PartialOrd, Ord)]
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
#[derive(Debug, Clone)]
pub struct ModuleImplementationScope {
	pub widths: HashMap<SourceSpan, BusWidth>,
	pub evaluated_expressions: HashMap<SourceSpan, EvaluatedEntry>,
	pub enriched_constants: HashMap<SourceSpan, crate::core::NumericConstant>,
	scopes: Vec<InternalScope>,
	is_generic: bool,
	api_ids: BiHashMap<InternalVariableId, SignalId>,
	internal_ids: HashMap<InternalVariableId, (usize, IdTableKey)>,
	variable_counter: usize,
	variables: HashMap<InternalVariableId, VariableDefined>,
	intermediate_signals: HashMap<usize, Vec<InternalVariableId>>,
}
pub trait InternalScopeTrait {
	fn contains_key(&self, key: &IdTableKey) -> bool;
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InternalScope {
	variables: HashMap<IdTableKey, InternalVariableId>,
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
	pub fn get_variable_by_id(&self, key: InternalVariableId) -> Option<VariableDefined> {
		self.variables.get(&key).map(|x| x.clone())
	}
	pub fn display_interface(&self, id_table: &IdTable) -> String {
		let mut s = String::new();
		let scope = self.scopes.first().unwrap();
		for (name, var) in &scope.variables {
			s += format!(
				"Variable {}: {:?}\n",
				id_table.get_value(name),
				self.variables.get(var).unwrap().var.kind
			)
			.as_str();
		}
		s += format!("dupa").as_str();

		s
	}
	pub fn get_interface_len(&self) -> usize {
		return self.scopes.first().unwrap().variables.len();
	}
	pub fn get_var(
		&self,
		scope_id: usize,
		name: &IdTableKey,
	) -> Result<&VariableDefined, crate::core::CompilerDiagnosticBuilder> {
		let scope = &self.scopes[scope_id];
		if let Some(variable) = scope.variables.get(name) {
			Ok(self.variables.get(variable).unwrap())
		}
		else {
			if let Some(parent_scope) = scope.parent_scope {
				self.get_var(parent_scope, name)
			}
			else {
				Err(SemanticError::VariableNotDeclared.to_diagnostic_builder())
			}
		}
	}
	pub fn add_expression(&mut self, span: SourceSpan, scope_id: usize, expr: crate::parser::ast::Expression) {
		self.evaluated_expressions
			.insert(span, EvaluatedEntry::new(expr, scope_id));
	}
	pub fn transorm_to_generic(&mut self) {
		return;
		// To be honest, I have no idea what was the true purpose of this method, I do not see any usecases now
		//debug!("Transforming scope to generic");
		//for scope in self.scopes.iter_mut() {
		//	for variable in scope.variables.values() {
		//		let var = self.variables.get_mut(variable).unwrap();
		//		match &mut var.var.kind {
		//			VariableKind::Signal(sig) => {
		//				sig.dimensions.iter_mut().for_each(|dim| {
		//					debug!("Transforming dimension {:?} to generic", dim);
		//					dim.to_generic();
		//					debug!("Transformed dimension {:?} to generic", dim);
		//				});
		//				use crate::analyzer::SignalType::*;
		//				match &mut sig.signal_type {
		//					Bus(bus) => match &mut bus.width {
		//						Some(w) => w.to_generic(),
		//						None => (),
		//					},
		//					Wire(_) => (),
		//					Auto(_) => unreachable!(),
		//				}
		//			},
		//			VariableKind::Generic(gen) => {
		//				match &mut gen.value {
		//					// unreachable!(),
		//					Some(val) => {
		//						debug!("Transforming value {:?} to generic", val);
		//						val.to_generic();
		//						debug!("Transformed value {:?} to generic", val);
		//					},
		//					None => (),
		//				}
		//			},
		//			VariableKind::ModuleInstance(_) => {
		//				unreachable!("Module instantion can't be declared in module implementation scope")
		//			},
		//		}
		//	}
		//}
	}
	pub fn unmark_as_generic(&mut self) {
		self.is_generic = false;
	}
	pub fn new() -> Self {
		Self {
			widths: HashMap::new(),
			evaluated_expressions: HashMap::new(),
			scopes: vec![InternalScope::new(None)],
			api_ids: BiHashMap::new(),
			variable_counter: 0,
			internal_ids: HashMap::new(),
			is_generic: false,
			enriched_constants: HashMap::new(),
			variables: HashMap::new(),
			intermediate_signals: HashMap::new(),
		}
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
			self.variables.get(variable)
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
	pub fn redeclare_variable(&mut self, mut var: VariableDefined) {
		let mut prev = self.variables.get(&var.id).unwrap().clone();
		let prev_copy = prev.clone();
		use VariableKind::*;
		match (&mut var.var.kind, &mut prev.var.kind) {
			(Signal(_), Signal(_)) => {
				info!("Redeclared variable {:?} to {:?}", prev, var);
				self.variables.insert(var.id, var);
			},
			(Generic(gen), Generic(gen1)) => {
				gen.is_wire = gen1.is_wire;
				info!("Redeclared variable {:?} to {:?}", prev, var);
				self.variables.insert(var.id, var);
			},
			(Signal(sig), Generic(gen)) => {
				gen.width = sig.width();
				info!("Redeclared generic variable {:?} to {:?}", prev_copy, prev);
				self.variables.insert(var.id, prev.clone());
			},
			(_, ModuleInstance(_)) | (ModuleInstance(_), _) | (Generic(_), Signal(_)) => panic!(),
		}
	}
	pub fn get_variable_in_scope(&self, scope_id: usize, key: &IdTableKey) -> Option<&VariableDefined> {
		let scope = &self.scopes[scope_id];
		scope.variables.get(key).and_then(|x| self.variables.get(x))
	}
	pub fn is_declared(&self, scope_key: usize, key: &IdTableKey) -> Option<SourceSpan> {
		self.scopes[scope_key]
			.variables
			.get(key)
			.map(|x| self.variables.get(x).unwrap().var.location)
	}
	pub fn insert_api_id(&mut self, id: InternalVariableId, api_id: SignalId) {
		self.api_ids.insert(id, api_id);
	}
	pub fn get_api_id(&self, scope_id: usize, key: &IdTableKey) -> Option<SignalId> {
		let scope = &self.scopes[scope_id];
		if let Some(variable) = scope.variables.get(key) {
			Some(self.api_ids.get_by_left(&variable).unwrap().clone())
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
	pub fn get_api_id_by_internal_id(&self, id: InternalVariableId) -> Option<SignalId> {
		self.api_ids.get_by_left(&id).map(|x| x.clone())
	}
	pub fn clear_scope(&mut self, scope_id: usize) {
		self.scopes[scope_id].variables.clear();
		let mut binding = Vec::new();
		self.intermediate_signals
			.get_mut(&scope_id)
			.unwrap_or(&mut binding)
			.clear();
	}
	pub fn define_variable(&mut self, scope_id: usize, mut var: Variable) -> miette::Result<InternalVariableId> {
		let id = InternalVariableId::new(self.variable_counter);
		var.kind.add_name_to_clock(id);
		self.variable_counter += 1;
		let name = var.name.clone();
		log::debug!(
			"Defining variable {:?} with name {:?} in scope {:?}",
			id,
			name,
			scope_id
		);
		let defined = VariableDefined {
			var,
			id,
			scope_id,
			is_iterated: false,
		};
		self.scopes[scope_id].variables.insert(name, id);
		self.variables.insert(id, defined);
		self.internal_ids.insert(id, (scope_id, name));
		Ok(id)
	}
	pub fn get_intermidiate_signal(&self, id: InternalVariableId) -> &VariableDefined {
		self.variables.get(&id).unwrap()
	}
	pub fn define_intermidiate_signal(
		&mut self,
		mut var: Variable,
		scope_id: usize,
	) -> miette::Result<InternalVariableId> {
		let id = InternalVariableId::new(self.variable_counter);
		var.kind.add_name_to_clock(id);
		log::debug!("Defining intermidiate signal {:?}", var);
		self.variable_counter += 1;
		let defined = VariableDefined {
			var,
			id,
			scope_id,
			is_iterated: false,
		};
		self.variables.insert(id, defined);
		if let Some(scope) = self.intermediate_signals.get_mut(&scope_id) {
			scope.push(id);
		}
		else {
			self.intermediate_signals.insert(scope_id, vec![id]);
		}
		Ok(id)
	}
	pub fn declare_variable(
		&mut self,
		mut var: Variable,
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
		var.kind.add_name_to_clock(id);
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
				None,
				handle
					.scope()
					.new_signal(id_table.get_by_key(&name).unwrap().as_str())
					.unwrap(),
			)?,
		);
		match &var.kind {
			VariableKind::Generic(_) => handle
				.expose(
					self.api_ids.get_by_left(&id).unwrap().clone(),
					hirn::design::SignalDirection::Input,
				)
				.unwrap(),
			VariableKind::Signal(sig) => match &sig.direction {
				super::Direction::Input(_) => handle
					.expose(
						self.api_ids.get_by_left(&id).unwrap().clone(),
						hirn::design::SignalDirection::Input,
					)
					.unwrap(),
				super::Direction::Output(_) => handle
					.expose(
						self.api_ids.get_by_left(&id).unwrap().clone(),
						hirn::design::SignalDirection::Output,
					)
					.unwrap(),
				_ => unreachable!("Only input and output signals can be declared in module implementation scope"),
			},
			VariableKind::ModuleInstance(_) => {
				unreachable!("Module instantion can't be declared in module implementation scope")
			},
		}
		let defined = VariableDefined {
			var,
			id,
			scope_id: 0,
			is_iterated: false,
		};
		self.scopes[0].variables.insert(name, defined.id);
		self.variables.insert(defined.id, defined);
		Ok(())
	}
	pub fn get_variable_location(&self, api_id: SignalId) -> SourceSpan {
		let id = self.api_ids.get_by_right(&api_id).unwrap();
		self.variables.get(id).unwrap().var.location
	}
	pub fn second_pass(&mut self, ctx: &mut GlobalAnalyzerContext, graph: &mut DependencyGraph) -> miette::Result<()> {
		for v in self.variables.values() {
			log::debug!("Checking variable {:?}", ctx.id_table.get_value(&v.var.name));
			if let VariableKind::Signal(sig) = &v.var.kind {
				if !sig.is_sensititivity_specified() {
					return Err(miette::Report::new(
						SemanticError::MissingSensitivityQualifier
							.to_diagnostic_builder()
							.label(
								v.var.location,
								"Signal must be either const, clock, comb, sync or async",
							)
							.build(),
					));
				}
				if !sig.is_signedness_specified() {
					return Err(miette::Report::new(
						SemanticError::MissingSignednessQualifier
							.to_diagnostic_builder()
							.label(v.var.location, "Bus signal must be either signed or unsigned")
							.build(),
					));
				}
				if !sig.is_width_specified() {
					return Err(miette::Report::new(
						SemanticError::WidthNotKnown
							.to_diagnostic_builder()
							.label(v.var.location, "Bus signals must have specified width")
							.build(),
					));
				}
				if sig.is_bus() && sig.is_clock() {
					return Err(miette::Report::new(
						SemanticError::ContradictingSpecifier
							.to_diagnostic_builder()
							.label(v.var.location, "Buses cannot be marked as clocks")
							.build(),
					));
				}

				let mut deps = sig.sensitivity.get_dependencies();
				use BusWidth::*;
				match sig.width().expect("Width not specified") {
					Evaluated(_) => (),
					EvaluatedLocated(_, loc) | Evaluable(loc) | WidthOf(loc) => {
						let entry = self.evaluated_expressions.get(&loc).unwrap();
						if !self.is_child_of(v.scope_id, entry.scope_id) {
							panic!("Variable redeclaration needed") // FIXME
						}
						deps.extend(entry.expression.get_dependencies(entry.scope_id, &self));
					},
				}
				graph.add_node(v.id);
				graph.add_edges(deps, v.id)
			}
			// we do not have to check for module instances, because their members are checked in previous pass
			if let VariableKind::Generic(gen) = &v.var.kind {
				if gen.value.is_none() && !gen.direction.is_input() {
					// emit warning
					let report = crate::core::CompilerDiagnosticBuilder::new_warning(
						"This generic variable is unitialized, it will not be emitted in the output file",
					)
					.label(v.location(), "This variable has no value")
					.build();
					ctx.diagnostic_buffer.push_diagnostic(report);
				}
				graph.add_node(v.id);
			}
		}
		Ok(())
	}
	pub fn is_child_of(&self, child: usize, parent: usize) -> bool {
		if child == parent {
			return true;
		}
		let mut current = child;
		while let Some(scope) = self.scopes.get(current) {
			match scope.parent_scope {
				Some(parent_scope) => {
					if parent_scope == parent {
						return true;
					}
					current = parent_scope;
				},
				None => return false,
			}
		}
		false
	}
	fn get_all_variables_declared_in_scope(&self, scope_id: usize) -> Vec<InternalVariableId> {
		let mut variables = Vec::new();
		let scope = self.scopes.get(scope_id).unwrap();
		variables.extend(scope.variables.values());
		variables.extend(self.intermediate_signals.get(&scope_id).unwrap_or(&Vec::new()));
		variables
	}
	pub fn register_all_variables_in_scope(
		&mut self,
		depedency_graph: &DependencyGraph,
		nc_table: &crate::core::NumericConstantTable,
		id_table: &crate::core::IdTable,
		additional_ctx: Option<&AdditionalContext>,
		scope_id: usize,
		scope_handle: &mut ScopeHandle,
	) {
		log::debug!("Registering all variables in scope {:?}", scope_id);
		let variables = self.get_all_variables_declared_in_scope(scope_id);
		for var in variables {
			depedency_graph.register(nc_table, id_table, additional_ctx, self, scope_id, scope_handle, var);
		}
	}
	pub fn register_variable(&mut self, id: InternalVariableId, scope: &mut ScopeHandle) {
		let var = self.variables.get(&id).unwrap();
		todo!("Registering variable {:?}", var);
	}
	pub fn is_already_registered(&self, id: InternalVariableId) -> bool {
		self.api_ids.contains_left(&id)
	}
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableDefined {
	pub var: Variable,
	pub id: InternalVariableId,
	pub scope_id: usize,
	pub is_iterated: bool,
}

impl VariableDefined {
	pub fn is_clock(&self) -> bool {
		self.var.is_clock()
	}
	pub fn get_sensitivity(&self) -> SignalSensitivity {
		match &self.var.kind {
			VariableKind::Signal(sig) => sig.sensitivity.clone(),
			VariableKind::Generic(gen) => SignalSensitivity::Const(gen.location),
			_ => unreachable!(),
		}
	}
	pub fn location(&self) -> SourceSpan {
		self.var.location
	}
}
