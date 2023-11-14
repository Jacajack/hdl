use std::{sync::Arc, collections::{HashSet, HashMap}};

use log::{error, info, debug};
use petgraph::graphmap::DiGraphMap;

use crate::{elab::{ElabAssumptions, ElabAssumptionsBase, ElabMessageKind}, design::{ScopeHandle, HasSensitivity, SignalId, Evaluates, ConditionalScope, RangeScope}};

use super::SignalGraphPassCtx;

impl SignalGraphPassCtx {
	/// Analyzes scope contents (non-recursively)
	fn elab_scope_content(
		&mut self,
		scope: ScopeHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		let pass_id = self.get_scope_pass_id(scope.id());
		debug!("Analyzing scope {:?} (pass {:?})", scope.id(), pass_id);
		debug!("Pass info: {:?}", self.pass_info.get(&pass_id));

		// Process all non-generic declarations
		for sig_id in scope.signals() {
			let sig = scope.design().get_signal(sig_id).unwrap();
			if !sig.is_generic() {
				self.declare_signal(sig_id, assumptions.clone())?;
			}
		}

		// Process all non-generic assignments
		for asmt in scope.assignments() {
			let driven_bits = asmt.lhs.try_drive_bits().ok_or(ElabMessageKind::NotDrivable)?;
			let sig_id = driven_bits.signal();
			let sig = scope.design().get_signal(sig_id).unwrap();
			if sig.is_generic() {
				continue;
			}

			// Special case - array assignment
			if sig.rank() > 0 && driven_bits.slice().rank() == 0 {
				self.drive_array(sig_id)?;
				// TODO validate matching array dimensions
				let rhs_depends = asmt.dependencies_bits();
				if let Some(rhs_array) = rhs_depends.first() {
					self.read_array(rhs_array.signal())?;
				}
				continue;
			}

			self.drive_signal(&driven_bits, assumptions.clone())?;

			let read = asmt.dependencies_bits();
			for range in &read {
				let read_sig_id = range.signal();
				let read_sig = scope.design().get_signal(read_sig_id).unwrap();
				if read_sig.is_generic() {
					continue;
				}

				self.read_signal(range, assumptions.clone())?;
			}
		}

		// TODO elab register
		// TODO elab submodules
		// TODO process submodule binding lists
		// TODO drive module input sigs
		// TODO read module output sigs
		Ok(())
	}

	/// Analyzes a scope and all its subscopes
	/// Note: pass must be registered before calling this function
	fn elab_scope(
		&mut self,
		scope: ScopeHandle,
		parent_assumptions: Arc<dyn ElabAssumptionsBase>,
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
			let lhs_sig = self
				.design
				.get_signal(lhs_target.signal)
				.expect("LHS signal not in design");

			// We ignore actual signal assignments
			if !lhs_sig.sensitivity().is_generic() {
				continue;
			}

			graph.add_node(lhs_target.signal);
			for dep_var in asmt.dependencies() {
				graph.add_edge(dep_var, lhs_target.signal, ());
			}

			if assigned_values.insert(lhs_target.signal, asmt.rhs.clone()).is_some() {
				error!(
					"The generic variable '{}' is assigned to more than once",
					lhs_sig.name()
				);
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
				Node { weight: signal_id } => {
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
		self.elab_scope_content(scope.clone(), assumptions_arc.clone())?;

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
			let handle = scope
				.design()
				.get_scope_handle(child_scope)
				.expect("scope not in design");
			self.elab_unconditional_scope(&handle, assumptions_arc.clone())?;
			visited_scopes.insert(child_scope);
		}

		Ok(())
	}

	fn elab_conditional_scope(
		&mut self,
		cond_scope: &ConditionalScope,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		// TODO validate condition expr

		let cond_value = cond_scope.condition.eval(&assumptions)?;
		if !cond_value.is_zero() {
			let scope_handle = self
				.design
				.get_scope_handle(cond_scope.scope)
				.expect("scope not in design");
			self.record_conditional_scope_pass(scope_handle.id(), true);
			self.elab_scope(scope_handle, assumptions)?;
		}
		else if let Some(else_scope_id) = cond_scope.else_scope {
			let scope_handle = self
				.design
				.get_scope_handle(else_scope_id)
				.expect("scope not in design");
			self.record_conditional_scope_pass(scope_handle.id(), false);
			self.elab_scope(scope_handle, assumptions)?;
		}
		Ok(())
	}

	fn elab_loop_scope(
		&mut self,
		range_scope: &RangeScope,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		let begin_val = range_scope.iterator_begin.eval(&assumptions)?;
		let end_val = range_scope.iterator_end.eval(&assumptions)?;
		let iter_count = (end_val - begin_val.clone()).try_into_i64()?;

		// TODO validate expressions

		if iter_count > self.config.max_for_iters {
			error!(
				"The for loop owning scope {:?} iterates more than {} times",
				range_scope.scope, self.config.max_for_iters
			);
			return Err(ElabMessageKind::MaxForIterCount);
		}

		// let iter_sig = self.design.get_signal(range_scope.iterator_var).expect("iterator signal not in design");
		let scope_handle = self
			.design
			.get_scope_handle(range_scope.scope)
			.expect("scope not in design");

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

	pub fn elab_unconditional_scope(
		&mut self,
		scope: &ScopeHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		self.record_scope_pass(scope.id());
		self.elab_scope(scope.clone(), assumptions)?;
		Ok(())
	}
}
