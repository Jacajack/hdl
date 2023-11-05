use std::collections::{HashSet, HashMap};
use std::sync::Arc;
use log::{info, error};
use petgraph::prelude::{DiGraph, DiGraphMap};

use crate::design::{ScopeHandle, ConditionalScope, RangeScope, Evaluates, DesignHandle, NumericConstant, HasSensitivity};
use crate::elab::{ElabSignal, ElabAssumptionsBase};
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
			info!("Found signal {} with width {:?}", sig.name(), width);
		}
	}

	fn analyze_unconditional_scope(&mut self, scope: ScopeHandle, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabError> {
		let mut graph = DiGraphMap::<SignalId, ()>::new();
		
		for asmt in scope.assignments() {
			let lhs_target = asmt.lhs.try_drive().ok_or(ElabError::NotDrivable)?;
			let lhs_sig = self.design.get_signal(lhs_target.signal).expect("LHS signal not in design");
			if !lhs_sig.sensitivity().is_generic() {
				continue;
			}

			graph.add_node(lhs_target.signal); 
			for rhs_var in asmt.rhs.get_variables() {
				graph.add_edge(rhs_var, lhs_target.signal, ());
			}

		}

		if petgraph::algo::is_cyclic_directed(&graph) {
			error!("The scope {:?} contains a cyclic generic dependency", scope.id());
			return Err(ElabError::CyclicGenericDependency);
		}

		for element in petgraph::algo::min_spanning_tree(&graph) {
			use petgraph::data::Element::*;
			match element {
				Node{weight} => {
					let sig = self.design.get_signal(weight).expect("signal not in design");
					info!("Resolving generic variable {}", sig.name());

				},
				_ => {},
			}
		}


		let mut visited_scopes = HashSet::new();

		for range_scope in scope.loop_subscopes() {
			self.analyze_for_scope(&range_scope, assumptions.clone())?;
			visited_scopes.insert(range_scope.scope);
		}

		for cond_scope in scope.conditional_subscopes() {
			self.analyze_if_scope(&cond_scope, assumptions.clone())?;
			visited_scopes.insert(cond_scope.scope);
		}

		for child_scope in scope.subscopes() {
			if visited_scopes.contains(&child_scope) {
				continue;
			}
			let handle = scope.design().get_scope_handle(child_scope).expect("scope not in design");
			self.analyze_unconditional_scope(handle, assumptions.clone())?;
			visited_scopes.insert(child_scope);
		}

		Ok(())
	}

	fn analyze_if_scope(&mut self, cond_scope: &ConditionalScope, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabError> {
		let cond_value = cond_scope.condition.eval(&assumptions)?;
		if !cond_value.is_zero() {
			let scope_handle = self.design.get_scope_handle(cond_scope.scope).expect("scope not in design");
			self.analyze_unconditional_scope(scope_handle, assumptions)?;
		}
		Ok(())
	}

	fn analyze_for_scope(&mut self, range_scope: &RangeScope, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabError> {
		let begin_val = range_scope.iterator_begin.eval(&assumptions)?;
		let end_val = range_scope.iterator_end.eval(&assumptions)?;
		let iter_count = (end_val - begin_val.clone()).try_into_i64()?;

		if iter_count > self.config.max_for_iters {
			return Err(ElabError::MaxForIterCount);
		}

		// let iter_sig = self.design.get_signal(range_scope.iterator_var).expect("iterator signal not in design");
		let scope_handle = self.design.get_scope_handle(range_scope.scope).expect("scope not in design");

		for i in 0..iter_count {
			let mut iter_assumption = ElabAssumptions::new_with_parent(assumptions.clone());
			iter_assumption.assume(range_scope.iterator_var, begin_val.clone() + i.into());
			let iter_arc: Arc<dyn ElabAssumptionsBase> = Arc::new(iter_assumption);
			// FIXME constant width
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

		ctx.analyze_unconditional_scope(module.scope(), full_ctx.assumptions())?;

		// for asmt in module.scope().assignments() {
		// 	let lhs_bits = asmt.lhs.try_drive_bits().expect("undrivable lhs");
		// 	let rhs_slices = asmt.rhs.get_used_slice_ranges();

		// 	// info!("Assignment to {:?}", lhs_bits);
		// 	// info!("Using slices {:?}", rhs_slices);
		// 	for slice in rhs_slices {
		// 		let sig = full_ctx.design().get_signal(slice.signal()).unwrap();
		// 		match slice.lsb_msb() {
		// 			Some((lsb, msb)) => {
		// 				let lsb_val = lsb.const_narrow_eval().unwrap();
		// 				let msb_val = msb.const_narrow_eval().unwrap();
		// 				info!("Marking {}[{}:{}] as used", sig.name(), lsb_val, msb_val);
		// 			}
		// 			None => info!("Marking {} as used", sig.name())
		// 		}
		// 	}
		// }

		full_ctx.generic_resolve_ctx = Some(ctx);
		Ok(full_ctx)
	}
}
