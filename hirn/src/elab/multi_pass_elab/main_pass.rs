mod assignment;
mod gen_signal;
mod instance_elab;
mod scope_elab;
mod scope_pass;
mod signal_drive;

use log::{debug, error, info};
use petgraph::graphmap::GraphMap;
use petgraph::Directed;
use std::collections::HashMap;
use std::sync::Arc;

use crate::design::{DesignHandle, ModuleHandle};
use crate::elab::{ElabAssumptionsBase, ElabMessageKind, ElabSignal};
use crate::{design::ScopeId, elab::ElabError};

use super::{
	full_elab::{FullElabCacheHandle, FullElabCtx},
	ElabPass,
};

pub use gen_signal::{GeneratedSignal, GeneratedSignalId, GeneratedSignalRef};
pub use scope_pass::{ScopePassId, ScopePassInfo};

#[derive(Clone, Debug, Copy)]
pub(super) struct MainPassConfig {
	pub max_for_iters: i64,
	pub max_signal_width: i64,
	pub max_array_dimension: i64,
	pub max_array_rank: usize,
	pub max_array_size: usize,
}

impl Default for MainPassConfig {
	fn default() -> Self {
		MainPassConfig {
			max_for_iters: 65536,
			max_signal_width: 65536,
			max_array_dimension: 65536,
			max_array_size: 65536,
			max_array_rank: 8,
		}
	}
}

pub struct MainPassResult {
	/// All generated signals
	signals: HashMap<GeneratedSignalId, GeneratedSignal>,

	/// Elaborated signals
	elab_signals: HashMap<GeneratedSignalRef, ElabSignal>,

	/// Scope pass info
	pass_info: HashMap<ScopePassId, ScopePassInfo>,

	/// Modules queued for elaboration
	queued_modules: Vec<(ModuleHandle, Arc<dyn ElabAssumptionsBase>)>,

	/// Combinational signal dependency graph
	comb_graph: CombGraph,
}

impl MainPassResult {
	pub fn signals(&self) -> &HashMap<GeneratedSignalId, GeneratedSignal> {
		&self.signals
	}

	pub fn elab_signals(&self) -> &HashMap<GeneratedSignalRef, ElabSignal> {
		&self.elab_signals
	}

	pub fn pass_info(&self) -> &HashMap<ScopePassId, ScopePassInfo> {
		&self.pass_info
	}

	pub fn queued_modules(&self) -> &[(ModuleHandle, Arc<dyn ElabAssumptionsBase>)] {
		&self.queued_modules
	}

	pub fn comb_graph(&self) -> &CombGraph {
		&self.comb_graph
	}
}

pub type CombGraph = GraphMap<GeneratedSignalRef, (), Directed>;

struct MainPassCtx {
	/// Design handle
	design: DesignHandle,

	/// Configuration for this pass
	config: MainPassConfig,

	/// Counter for scope passes
	scope_pass_counter: usize,

	/// Module instance counter
	instance_counter: usize,

	/// Current scope pass ID recorded for each scope
	current_pass: HashMap<ScopeId, ScopePassId>,

	/// Auxiliary information about each scope pass
	pass_info: HashMap<ScopePassId, ScopePassInfo>,

	/// All generated signals
	signals: HashMap<GeneratedSignalId, GeneratedSignal>,

	/// Elaborated signals
	elab_signals: HashMap<GeneratedSignalRef, ElabSignal>,

	/// Combinational signal dependency graph
	comb_graph: CombGraph,

	/// Modules queued for elaboration
	queued_modules: Vec<(ModuleHandle, Arc<dyn ElabAssumptionsBase>)>,
}

impl MainPassCtx {
	fn new(design: DesignHandle, config: MainPassConfig) -> Self {
		MainPassCtx {
			config,
			design,
			scope_pass_counter: 0,
			instance_counter: 0,
			// signals: HashMap::new(),
			comb_graph: GraphMap::new(),
			// clock_graph: GraphMap::new(),
			// clock_groups: HashMap::new(),
			pass_info: HashMap::new(),
			current_pass: HashMap::new(),

			signals: HashMap::new(),
			elab_signals: HashMap::new(),

			queued_modules: Vec::new(),
		}
	}

	fn elab_module(
		&mut self,
		module: ModuleHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		assert!(assumptions.design().is_some());
		info!("Elaborating module {:?}", module.id());
		debug!("Assumptions: {:?}", assumptions);
		self.elab_unconditional_scope(&module.scope(), assumptions.clone())?;
		Ok(())
	}
}

pub(super) struct MainPass;

impl ElabPass<FullElabCtx, FullElabCacheHandle> for MainPass {
	fn name(&self) -> &'static str {
		"SignalGraphPass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running signal graph pass...");
		let mut ctx = MainPassCtx::new(full_ctx.design().clone(), full_ctx.main_pass_config);
		let module = full_ctx.module_handle();

		let result = ctx.elab_module(module.clone(), full_ctx.assumptions());
		if let Err(err) = result {
			error!("Module {:?} contains errors ({:?})", module.id(), err);
			full_ctx.add_message(err);
		}

		info!("Initial elab phase for {:?} complete", module.id());
		info!("Generated signals registered: {}", ctx.signals.len());
		info!("Signal graph node count: {}", ctx.comb_graph.node_count());
		info!("Signal graph edge count: {}", ctx.comb_graph.edge_count());
		info!("Elab signals registered: {}", ctx.elab_signals.len());

		full_ctx.main_pass_result = Some(MainPassResult {
			signals: ctx.signals,
			elab_signals: ctx.elab_signals,
			pass_info: ctx.pass_info,
			queued_modules: ctx.queued_modules,
			comb_graph: ctx.comb_graph,
		});
		Ok(full_ctx)
	}
}
