use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use log::{info, error, debug};
use petgraph::prelude::DiGraphMap;

use crate::design::{ScopeHandle, ConditionalScope, RangeScope, Evaluates, DesignHandle, HasSensitivity};
use crate::elab::{ElabAssumptionsBase, ElabMessageKind};
use crate::{elab::{ElabError, ElabAssumptions}, design::{ScopeId, SignalId}};

use super::{full_elab::{FullElabCtx, FullElabCacheHandle}, ElabPass};

#[derive(Clone, Debug, Copy)]
pub(super) struct SignalGraphPassConfig {
	pub max_for_iters: i64,
	pub _max_signal_width: i64,
	pub _max_array_size: i64,
	pub _max_array_rank: i64,
}

impl Default for SignalGraphPassConfig {
	fn default() -> Self {
		SignalGraphPassConfig {
			max_for_iters: 65536,
			_max_signal_width: 65536,
			_max_array_size: 65536,
			_max_array_rank: 8,
		}
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedSignalId {
	id: SignalId,
	pass_id: ScopePassId,
}

/// IDs generated each time a scope is visited
#[derive(Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct ScopePassId(usize);

#[derive(Clone, Copy, Debug)]
pub enum ScopePassInfo {
	Unconditional(ScopeId),
	Conditional {
		id: ScopeId,
		was_true: bool,
	},
	Range {
		id: ScopeId,
		iter_number: i64,
	}
}
pub(super) struct SignalGraphPassResult {
	// signals: HashMap<GeneratedSignalId, ElabSignal>,
	// comb_graph: GraphMap<GeneratedSignalId, (), Directed>,
	// clock_graph: GraphMap<GeneratedSignalId, (), Undirected>,
	// clock_groups: HashMap<GeneratedSignalId, usize>,
	// pass_info: HashMap<usize, ScopePassInfo>,
}

struct SignalGraphPassCtx {
	config: SignalGraphPassConfig,
	design: DesignHandle,

	scope_pass_counter: usize,
	current_pass: HashMap<ScopeId, ScopePassId>,
	pass_info: HashMap<ScopePassId, ScopePassInfo>,


	// signals: HashMap<GeneratedSignalId, ElabSignal>,
	// comb_graph: GraphMap<GeneratedSignalId, (), Directed>,
	// clock_graph: GraphMap<GeneratedSignalId, (), Undirected>,
	// clock_groups: HashMap<GeneratedSignalId, usize>,
}

impl SignalGraphPassCtx {
	fn new(design: DesignHandle, config: SignalGraphPassConfig) -> Self {
		SignalGraphPassCtx {
			config,
			design,
			scope_pass_counter: 0,
			// signals: HashMap::new(),
			// comb_graph: GraphMap::new(),
			// clock_graph: GraphMap::new(),
			// clock_groups: HashMap::new(),
			pass_info: HashMap::new(),
			current_pass: HashMap::new(),

		}
	}

	/// Records that an unconditional scope has been visited
	fn record_scope_pass(&mut self, id: ScopeId) -> ScopePassId {
		let pass_id = ScopePassId(self.scope_pass_counter);
		self.scope_pass_counter += 1;
		self.pass_info.insert(pass_id, ScopePassInfo::Unconditional(id));
		self.current_pass.insert(id, pass_id);
		pass_id
	}

	/// Records that a conditional scope has been visited
	fn record_conditional_scope_pass(&mut self, id: ScopeId, cond_true: bool) -> ScopePassId {
		let pass_id = ScopePassId(self.scope_pass_counter);
		self.scope_pass_counter += 1;
		self.pass_info.insert(pass_id, ScopePassInfo::Conditional {
			id,
			was_true: cond_true,
		});
		self.current_pass.insert(id, pass_id);
		pass_id
	}

	/// Recrods a pass through a range scope
	fn record_range_scope_pass(&mut self, id: ScopeId, iter: i64) -> ScopePassId {
		let pass_id = ScopePassId(self.scope_pass_counter);
		self.scope_pass_counter += 1;
		self.pass_info.insert(pass_id, ScopePassInfo::Range {
			id,
			iter_number: iter,
		});
		self.current_pass.insert(id, pass_id);
		pass_id
	}

	/// Returns current pass ID for a scope
	fn get_scope_pass_id(&self, id: ScopeId) -> ScopePassId {
		self.current_pass.get(&id).copied().expect("scope pass not recorded!")
	}

	/// Returns a generated signal ID based on design signal ID and scope pass ID
	fn get_generated_signal_id(&self, id: SignalId) -> GeneratedSignalId {
		let sig = self.design.get_signal(id).expect("signal not in design");

		GeneratedSignalId {
			id,
			pass_id: self.get_scope_pass_id(sig.parent_scope),
		}
	}

	/// Analyzes scope contents (non-recursively)
	fn elab_scope_content(
		&mut self,
		scope: ScopeHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>
	) {
		let pass_id = self.get_scope_pass_id(scope.id());
		debug!("Analyzing scope {:?} (pass {:?})", scope.id(), pass_id);
		debug!("Pass info: {:?}", self.pass_info.get(&pass_id));

		for sig_id in scope.signals() {
			let sig = scope.design().get_signal(sig_id).unwrap();
			let width = sig.width().eval(&assumptions).unwrap();

			// TODO validate all expressions

			info!("Found signal {} with width {} - generated ID {:?}", sig.name(), width, self.get_generated_signal_id(sig_id));
		}
	}

	/// Analyzes a scope and all its subscopes
	/// Note: pass must be registered before calling this function
	fn elab_scope(
		&mut self,
		scope: ScopeHandle,
		parent_assumptions: Arc<dyn ElabAssumptionsBase>
	) -> Result<(), ElabMessageKind> {
		let mut assumptions = ElabAssumptions::new_with_parent(parent_assumptions);
		let mut assigned_values = HashMap::new();
		let mut graph = DiGraphMap::<SignalId, ()>::new();
		
		// Add all generic signals to the graph
		for sig_id in scope.signals() {
			let sig = scope.design().get_signal(sig_id).unwrap();
			if sig.is_generic() {
				graph.add_node(sig_id);
			}
		}

		// Process all assignments
		for asmt in scope.assignments() {
			let lhs_target = asmt.lhs.try_drive().ok_or(ElabMessageKind::NotDrivable)?;
			let lhs_sig = self.design.get_signal(lhs_target.signal).expect("LHS signal not in design");
			
			// We ignore actual signal assignments
			if !lhs_sig.sensitivity().is_generic() {
				continue;
			}

			graph.add_node(lhs_target.signal); 
			for dep_var in asmt.dependencies() {
				graph.add_edge(dep_var, lhs_target.signal, ());
			}

			if assigned_values.insert(lhs_target.signal, asmt.rhs.clone()).is_some() {
				error!("The generic variable '{}' is assigned to more than once", lhs_sig.name());
				return Err(ElabMessageKind::MultipleGenericAssignments);
			}
		}

		// Cycles are a no-no
		if petgraph::algo::is_cyclic_directed(&graph) {
			error!("The scope {:?} contains a cyclic generic dependency", scope.id());
			return Err(ElabMessageKind::CyclicGenericDependency);
		}

		// Resolve all generic values
		for element in petgraph::algo::min_spanning_tree(&graph) {
			use petgraph::data::Element::*;
			match element {
				Node{weight: signal_id} => {
					// If the variable has an assumption already, we don't need to resolve it
					if assumptions.get(signal_id).is_some() {
						continue;
					}
					
					let sig = self.design.get_signal(signal_id).expect("signal not in design");
					let expr = assigned_values.get(&signal_id).ok_or_else(|| {
						error!("The generic variable '{}' is not assigned", sig.name());
						ElabMessageKind::UnassignedGeneric
					})?;
					let ass: &dyn ElabAssumptionsBase = &assumptions;
					let value = expr.eval(&ass)?; // FIXME check if eval result is compatible with the signal
					info!("Generic var '{}' assumed to be {}", sig.name(), value);
					assumptions.assume(signal_id, value);
				},
				_ => {},
			}
		}

		// Analyze resolved scope
		let assumptions_arc = Arc::new(assumptions);
		self.elab_scope_content(
			scope.clone(),
			assumptions_arc.clone()
		);

		// Recurse for subscopes
		let mut visited_scopes = HashSet::new();

		for range_scope in scope.loop_subscopes() {
			self.elab_loop_scope(&range_scope, assumptions_arc.clone())?;
			visited_scopes.insert(range_scope.scope);
		}

		for cond_scope in scope.conditional_subscopes() {
			self.elab_conditional_scope(&cond_scope, assumptions_arc.clone())?;
			visited_scopes.insert(cond_scope.scope);
		}

		for child_scope in scope.subscopes() {
			if visited_scopes.contains(&child_scope) {
				continue;
			}
			let handle = scope.design().get_scope_handle(child_scope).expect("scope not in design");
			self.elab_unconditional_scope(&handle, assumptions_arc.clone())?;
			visited_scopes.insert(child_scope);
		}

		Ok(())
	}

	fn elab_conditional_scope(&mut self, cond_scope: &ConditionalScope, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
		// TODO validate condition expr
		
		let cond_value = cond_scope.condition.eval(&assumptions)?;
		if !cond_value.is_zero() {
			let scope_handle = self.design.get_scope_handle(cond_scope.scope).expect("scope not in design");
			self.record_conditional_scope_pass(scope_handle.id(), true);
			self.elab_scope(scope_handle, assumptions)?;
		}
		else if let Some(else_scope_id) = cond_scope.else_scope{
			let scope_handle = self.design.get_scope_handle(else_scope_id).expect("scope not in design");
			self.record_conditional_scope_pass(scope_handle.id(), false);
			self.elab_scope(scope_handle, assumptions)?;
		}
		Ok(())
	}

	fn elab_loop_scope(&mut self, range_scope: &RangeScope, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
		let begin_val = range_scope.iterator_begin.eval(&assumptions)?;
		let end_val = range_scope.iterator_end.eval(&assumptions)?;
		let iter_count = (end_val - begin_val.clone()).try_into_i64()?;

		// TODO validate expressions

		if iter_count > self.config.max_for_iters {
			error!("The for loop owning scope {:?} iterates more than {} times", range_scope.scope, self.config.max_for_iters);
			return Err(ElabMessageKind::MaxForIterCount);
		}

		// let iter_sig = self.design.get_signal(range_scope.iterator_var).expect("iterator signal not in design");
		let scope_handle = self.design.get_scope_handle(range_scope.scope).expect("scope not in design");

		for i in 0..=iter_count {
			let mut iter_assumption = ElabAssumptions::new_with_parent(assumptions.clone());
			iter_assumption.assume(range_scope.iterator_var, begin_val.clone() + i.into());
			let iter_arc: Arc<dyn ElabAssumptionsBase> = Arc::new(iter_assumption);
			// FIXME numeric constant width
			self.record_range_scope_pass(scope_handle.id(), i);
			self.elab_scope(scope_handle.clone(), iter_arc)?;
		}

		Ok(())
	}

	fn elab_unconditional_scope(&mut self, scope: &ScopeHandle, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
		self.record_scope_pass(scope.id());
		self.elab_scope(scope.clone(), assumptions)?;
		Ok(())
	}
}

pub(super) struct SignalGraphPass;

impl ElabPass<FullElabCtx, FullElabCacheHandle> for SignalGraphPass {
	fn name(&self) -> &'static str {
		"GenericResolvePass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running generic resolution pass...");
		let mut ctx = SignalGraphPassCtx::new(full_ctx.design().clone(), full_ctx.sig_graph_config.clone());
		let module = full_ctx.module_handle();

		let result = ctx.elab_unconditional_scope(&module.scope(), full_ctx.assumptions());
		if let Err(err) = result {
			error!("Module {:?} contains errors ({:?})", module.id(), err);
			full_ctx.add_message(err.into());
		}

		full_ctx.sig_graph_result = Some(SignalGraphPassResult{}); // FIXME
		Ok(full_ctx)
	}
}
