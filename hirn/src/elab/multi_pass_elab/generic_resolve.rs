use std::collections::{HashSet, HashMap};
use std::sync::Arc;
use log::{info, error};
use petgraph::prelude::{DiGraph, DiGraphMap};

use crate::design::{ScopeHandle, ConditionalScope, RangeScope, Evaluates, DesignHandle, NumericConstant, HasSensitivity};
use crate::elab::{ElabSignal, ElabAssumptionsBase, ElabMessageKind};
use crate::{elab::{ElabError, ElabAssumptions}, design::{ScopeId, SignalId, NarrowEval}};
use crate::elab::GenericVar;

use super::{full_elab::{FullElabCtx, FullElabCacheHandle}, ElabPass};

struct PassConfig {
	max_for_iters: i64,
	max_signal_width: i64,
	max_array_size: i64,
	max_array_rank: i64,
}

impl Default for PassConfig {
	fn default() -> Self {
		PassConfig {
			max_for_iters: 65536,
			max_signal_width: 65536,
			max_array_size: 65536,
			max_array_rank: 8,
		}
	}
}

#[derive()]
pub(super) struct GenericResolvePassCtx {
	config: PassConfig,
	design: DesignHandle,
	signals: HashMap<(SignalId, usize), ElabSignal>,
	assumptions: HashMap<(SignalId, Vec<GenericVar>, usize), GenericVar>,
}

impl GenericResolvePassCtx {
	fn new(design: DesignHandle) -> Self {
		GenericResolvePassCtx {
			config: PassConfig::default(),
			design,
			signals: HashMap::new(),
			assumptions: HashMap::new(),
		}
	}

	fn analyze_resolved_scope(&mut self, scope: ScopeHandle, assumptions: Arc<dyn ElabAssumptionsBase>) {
		for sig_id in scope.signals() {
			let sig = scope.design().get_signal(sig_id).unwrap();
			let width = sig.width().eval(&assumptions).unwrap();

			// TODO validate all expressions

			info!("Found signal {} with width {}", sig.name(), width);
		}
	}

	fn analyze_unconditional_scope(&mut self, scope: ScopeHandle, parent_assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
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
					let value = expr.eval(&ass)?;
					info!("Generic var '{}' assumed to be {}", sig.name(), value);
					assumptions.assume(signal_id, value);
				},
				_ => {},
			}
		}

		// Analyze resolved scope
		let assumptions_arc = Arc::new(assumptions);
		self.analyze_resolved_scope(scope.clone(), assumptions_arc.clone());

		// Recurse for subscopes
		let mut visited_scopes = HashSet::new();

		for range_scope in scope.loop_subscopes() {
			self.analyze_for_scope(&range_scope, assumptions_arc.clone())?;
			visited_scopes.insert(range_scope.scope);
		}

		for cond_scope in scope.conditional_subscopes() {
			self.analyze_if_scope(&cond_scope, assumptions_arc.clone())?;
			visited_scopes.insert(cond_scope.scope);
		}

		for child_scope in scope.subscopes() {
			if visited_scopes.contains(&child_scope) {
				continue;
			}
			let handle = scope.design().get_scope_handle(child_scope).expect("scope not in design");
			self.analyze_unconditional_scope(handle, assumptions_arc.clone())?;
			visited_scopes.insert(child_scope);
		}

		Ok(())
	}

	fn analyze_if_scope(&mut self, cond_scope: &ConditionalScope, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
		// TODO validate condition expr
		
		let cond_value = cond_scope.condition.eval(&assumptions)?;
		if !cond_value.is_zero() {
			let scope_handle = self.design.get_scope_handle(cond_scope.scope).expect("scope not in design");
			self.analyze_unconditional_scope(scope_handle, assumptions)?;
		}
		else if let Some(else_scope_id) = cond_scope.else_scope{
			let scope_handle = self.design.get_scope_handle(else_scope_id).expect("scope not in design");
			self.analyze_unconditional_scope(scope_handle, assumptions)?;
		}
		Ok(())
	}

	fn analyze_for_scope(&mut self, range_scope: &RangeScope, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
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

		for i in 0..iter_count {
			let mut iter_assumption = ElabAssumptions::new_with_parent(assumptions.clone());
			iter_assumption.assume(range_scope.iterator_var, begin_val.clone() + i.into());
			let iter_arc: Arc<dyn ElabAssumptionsBase> = Arc::new(iter_assumption);
			// FIXME numeric constant width
			self.analyze_unconditional_scope(scope_handle.clone(), iter_arc)?;
		}

		Ok(())
	}
}

pub(super) struct GenericResolvePass;

impl ElabPass<FullElabCtx, FullElabCacheHandle> for GenericResolvePass {
	fn name(&self) -> &'static str {
		"GenericResolvePass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running generic resolution pass...");
		let mut ctx = GenericResolvePassCtx::new(full_ctx.design().clone());
		let module = full_ctx.module_handle();

		let result = ctx.analyze_unconditional_scope(module.scope(), full_ctx.assumptions());
		if let Err(err) = result {
			error!("Module {:?} contains errors ({:?})", module.id(), err);
			full_ctx.add_message(err.into());
		}

		full_ctx.generic_resolve_ctx = Some(ctx);
		Ok(full_ctx)
	}
}
