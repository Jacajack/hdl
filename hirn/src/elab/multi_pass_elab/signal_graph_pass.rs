use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use log::{info, error, debug};
use petgraph::Directed;
use petgraph::prelude::{DiGraphMap, GraphMap};

use crate::design::{ScopeHandle, ConditionalScope, RangeScope, Evaluates, DesignHandle, HasSensitivity, SignalSlice, SignalSliceRange, ModuleHandle};
use crate::elab::{ElabAssumptionsBase, ElabMessageKind, ElabSignal};
use crate::{elab::{ElabError, ElabAssumptions}, design::{ScopeId, SignalId}};

use super::{full_elab::{FullElabCtx, FullElabCacheHandle}, ElabPass};

#[derive(Clone, Debug, Copy)]
pub(super) struct SignalGraphPassConfig {
	pub max_for_iters: i64,
	pub max_signal_width: i64,
	pub max_array_dimension: i64,
	pub max_array_rank: usize,
	pub max_array_size: usize,
}

impl Default for SignalGraphPassConfig {
	fn default() -> Self {
		SignalGraphPassConfig {
			max_for_iters: 65536,
			max_signal_width: 65536,
			max_array_dimension: 65536,
			max_array_size: 65536,
			max_array_rank: 8,
		}
	}
}

/// Signal ID coupled with scope pass ID to distingush between generated signals
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedSignalId {
	id: SignalId,
	pass_id: ScopePassId,
}

/// References specific field of a generated signal
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedSignalRef {
	id: GeneratedSignalId,
	index: Option<u32>,	
}

/// Represents a generated signal (width + dimensions evaluated)
pub struct GeneratedSignal {
	width: u32,
	dimensions: Vec<usize>,
}

impl GeneratedSignal {

}

/// IDs generated each time a scope is visited
#[derive(Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct ScopePassId(usize);

/// Auxiliary information about a scope pass 
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
pub struct SignalGraphPassResult {
	/// All generated signals
	signals: HashMap<GeneratedSignalId, GeneratedSignal>,

	/// Elaborated signals
	elab_signals: HashMap<GeneratedSignalRef, ElabSignal>,
}

impl SignalGraphPassResult {
	pub fn signals(&self) -> &HashMap<GeneratedSignalId, GeneratedSignal> {
		&self.signals
	}

	pub fn elab_signals(&self) -> &HashMap<GeneratedSignalRef, ElabSignal> {
		&self.elab_signals
	}
}

struct SignalSliceRangeEvalResult<'a> {
	elab_sig: &'a mut ElabSignal,
	lsb_msb: Option<(i64, i64)>,
}

struct SignalGraphPassCtx {
	/// Design handle
	design: DesignHandle,

	/// Configuration for this pass
	config: SignalGraphPassConfig,

	/// Counter for scope passes
	scope_pass_counter: usize,

	/// Current scope pass ID recorded for each scope
	current_pass: HashMap<ScopeId, ScopePassId>,

	/// Auxiliary information about each scope pass
	pass_info: HashMap<ScopePassId, ScopePassInfo>,

	/// All generated signals
	signals: HashMap<GeneratedSignalId, GeneratedSignal>,

	/// Elaborated signals
	elab_signals: HashMap<GeneratedSignalRef, ElabSignal>,

	comb_graph: GraphMap<GeneratedSignalRef, (), Directed>,

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
			comb_graph: GraphMap::new(),
			// clock_graph: GraphMap::new(),
			// clock_groups: HashMap::new(),
			pass_info: HashMap::new(),
			current_pass: HashMap::new(),

			signals: HashMap::new(),
			elab_signals: HashMap::new(),

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

	fn get_generated_signal_ref(&self, slice: &SignalSlice, assumptions: Arc<dyn ElabAssumptionsBase>) -> GeneratedSignalRef {
		let gen_id = self.get_generated_signal_id(slice.signal);
		if slice.indices.is_empty() {
			GeneratedSignalRef {
				id: gen_id,
				index: None,
			}
		}
		else {
			todo!("Driving array signals not implemented yet");

			// GeneratedSignalRef {
			// 	id: gen_id,
			// 	index: None
			// }
		}
	}

	fn declare_signal(&mut self, id: SignalId, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<GeneratedSignalId, ElabMessageKind> {
		let sig = self.design.get_signal(id).expect("signal not in design");
		let pass_id = self.get_scope_pass_id(sig.parent_scope);
		assert!(!sig.is_generic());

		debug!("Signal '{}' declared in pass {:?}", sig.name(), pass_id);

		// Check signal rank 
		if sig.rank() > self.config.max_array_rank {
			error!("Signal {} has rank {} which is out of range", sig.name(), sig.rank());
			return Err(ElabMessageKind::InvalidArrayRank(sig.rank() as u32));
		}

		// Evaluate width & validate range
		let width = sig.width().eval(&assumptions)?.try_into_i64()?;
		if width < 0 || width > self.config.max_signal_width {
			error!("Signal {} has width {} which is out of range", sig.name(), width);
			return Err(ElabMessageKind::InvalidSignalWidth(width));
		}

		// Evaluate dimensions
		let mut dimensions = Vec::new();
		for dim_expr in &sig.dimensions {
			let dim = dim_expr.eval(&assumptions)?.try_into_i64()?;
			if dim < 0 || dim > self.config.max_array_dimension {
				error!("Signal {} has dimension {} which is out of range", sig.name(), dim);
				return Err(ElabMessageKind::InvalidArrayDimension(dim));
			}
			dimensions.push(dim as usize);
		}

		// Verify total array size
		let total_fields = dimensions.iter().product::<usize>();
		if total_fields > self.config.max_array_size {
			error!("Signal {} has {} fields which is out of range", sig.name(), total_fields);
			return Err(ElabMessageKind::InvalidArraySize(total_fields));
		}

		// ID for the generated signal
		let gen_id = GeneratedSignalId {
			id,
			pass_id,
		};

		// Insert all array fields into the graph and elab signal array
		if dimensions.len() > 0 {
			debug!("Array signal - will insert total of {} signal nodes", total_fields);
			for i in 0..total_fields {
				let sig_ref = GeneratedSignalRef {
					id: gen_id,
					index: Some(i as u32),
				};

				self.comb_graph.add_node(sig_ref);
				let sig_existed = self.elab_signals.insert(sig_ref, ElabSignal::new(width as u32)).is_some();
				assert!(!sig_existed, "An elab signal already exists for this generated signal ref")
			}
		}
		else {
			debug!("Scalar signal - inserting into graph and registering elab signals");
			let sig_ref = GeneratedSignalRef {
				id: gen_id,
				index: None,
			};

			self.comb_graph.add_node(sig_ref);
			let sig_existed = self.elab_signals.insert(sig_ref, ElabSignal::new(width as u32)).is_some();
			assert!(!sig_existed, "An elab signal already exists for this generated signal ref")
		}

		// Register the generated signal
		let gen_sig = GeneratedSignal {
			width: width as u32,
			dimensions,
		};

		let sig_exists = self.signals.insert(gen_id.clone(), gen_sig).is_some();
		assert!(!sig_exists, "Generated signal already exists!");

		Ok(gen_id)

	}

	fn eval_slice_range(&mut self, range: SignalSliceRange, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<SignalSliceRangeEvalResult, ElabMessageKind> {
		let sig_ref = self.get_generated_signal_ref(range.slice(), assumptions.clone());
		let elab_sig = self.elab_signals.get_mut(&sig_ref).expect("Sliced signal not registered in elab");
		
		if range.is_full() {
			Ok(SignalSliceRangeEvalResult {
				elab_sig,
				lsb_msb: None,
			})
		} else {
			let lsb_expr = range.lsb().unwrap();
			let msb_expr = range.msb().unwrap();
			let lsb_val = lsb_expr.eval(&assumptions)?;
			let msb_val = msb_expr.eval(&assumptions)?;
			let lsb = lsb_val.try_into_i64()?;
			let msb = msb_val.try_into_i64()?;

			if lsb < 0 || msb < 0 || msb < lsb || msb >= (elab_sig.width() as i64) {
				error!("Signal {:?} (width {}) is driven with invalid range {}:{}", range.signal(), elab_sig.width(), msb, lsb);
				return Err(ElabMessageKind::InvalidSignalBitRange);
			}

			Ok(SignalSliceRangeEvalResult{
				elab_sig,
				lsb_msb: Some((lsb, msb)),
			})
		}
	}

	fn drive_signal(&mut self, range: &SignalSliceRange, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
		debug!("Driving signal {:?}", range);
		
		let result = self.eval_slice_range(range.clone(), assumptions)?;		
		if let Some((lsb, msb)) = result.lsb_msb {
			result.elab_sig.drive_bits(lsb as u32, msb as u32);
		} else {
			result.elab_sig.drive();
		}

		Ok(())
	}

	fn read_signal(&mut self, range: &SignalSliceRange, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
		debug!("Marking signal as read {:?}", range);
		
		let result = self.eval_slice_range(range.clone(), assumptions)?;		
		if let Some((lsb, msb)) = result.lsb_msb {
			result.elab_sig.read_bits(lsb as u32, msb as u32);
		} else {
			result.elab_sig.read();
		}

		Ok(())
	}

	fn elab_module_interface(&mut self, module: ModuleHandle, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
		for interface_sig in module.interface() {
			let sig_id = interface_sig.signal;
			let sig = module.design().get_signal(sig_id).unwrap();
			if sig.is_generic() {
				continue;
			}

			if interface_sig.is_input() {
				self.drive_signal(&sig_id.into(), assumptions.clone())?;
			}
			else {
				self.read_signal(&sig_id.into(), assumptions.clone())?;
			}
		}

		Ok(())
	}

	/// Analyzes scope contents (non-recursively)
	fn elab_scope_content(
		&mut self,
		scope: ScopeHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>
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
		)?;

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

	fn elab_module(&mut self, module: ModuleHandle, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<(), ElabMessageKind> {
		self.elab_unconditional_scope(&module.scope(), assumptions.clone())?;
		self.elab_module_interface(module, assumptions.clone())?;
		Ok(())
	}

}

pub(super) struct SignalGraphPass;

impl ElabPass<FullElabCtx, FullElabCacheHandle> for SignalGraphPass {
	fn name(&self) -> &'static str {
		"SignalGraphPass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running signal graph pass...");
		let mut ctx = SignalGraphPassCtx::new(full_ctx.design().clone(), full_ctx.sig_graph_config.clone());
		let module = full_ctx.module_handle();

		let result = ctx.elab_module(module.clone(), full_ctx.assumptions());
		if let Err(err) = result {
			error!("Module {:?} contains errors ({:?})", module.id(), err);
			full_ctx.add_message(err.into());
		}

		info!("Initial elab phase for {:?} complete", module.id());
		info!("Generated signals registered: {}", ctx.signals.len());
		info!("Signal graph node count: {}", ctx.comb_graph.node_count());
		info!("Signal graph edge count: {}", ctx.comb_graph.edge_count());
		info!("Elab signals registered: {}", ctx.elab_signals.len());

		full_ctx.sig_graph_result = Some(SignalGraphPassResult{
			signals: ctx.signals,
			elab_signals: ctx.elab_signals,
		}); 
		Ok(full_ctx)
	}
}
