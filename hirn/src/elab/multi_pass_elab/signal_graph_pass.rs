mod gen_signal;
mod instance_elab;
mod scope_elab;
mod scope_pass;
mod signal_drive;

use log::{error, info};
use petgraph::graphmap::GraphMap;
use petgraph::Directed;
use std::collections::HashMap;
use std::sync::Arc;

use crate::design::{DesignHandle, HasSensitivity, ModuleHandle};
use crate::elab::{ElabAssumptionsBase, ElabMessageKind, ElabSignal};
use crate::{design::ScopeId, elab::ElabError};

use super::{
	full_elab::{FullElabCacheHandle, FullElabCtx},
	ElabPass,
};

pub use gen_signal::{GeneratedSignal, GeneratedSignalId, GeneratedSignalRef};
pub use scope_pass::{ScopePassId, ScopePassInfo};

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

pub struct SignalGraphPassResult {
	/// All generated signals
	signals: HashMap<GeneratedSignalId, GeneratedSignal>,

	/// Elaborated signals
	elab_signals: HashMap<GeneratedSignalRef, ElabSignal>,

	/// Scope pass info
	pass_info: HashMap<ScopePassId, ScopePassInfo>,
}

impl SignalGraphPassResult {
	pub fn signals(&self) -> &HashMap<GeneratedSignalId, GeneratedSignal> {
		&self.signals
	}

	pub fn elab_signals(&self) -> &HashMap<GeneratedSignalRef, ElabSignal> {
		&self.elab_signals
	}

	pub fn pass_info(&self) -> &HashMap<ScopePassId, ScopePassInfo> {
		&self.pass_info
	}
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

	fn elab_module_interface(
		&mut self,
		module: ModuleHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		for interface_sig in module.interface() {
			let sig_id = interface_sig.signal;
			let sig = module.design().get_signal(sig_id).unwrap();
			if sig.is_generic() {
				continue;
			}

			match interface_sig.is_input() {
				true => self.drive_signal(&sig_id.into(), assumptions.clone()),
				false => self.read_signal(&sig_id.into(), assumptions.clone()),
			}?;
		}

		Ok(())
	}

	fn elab_module(
		&mut self,
		module: ModuleHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
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

		full_ctx.sig_graph_result = Some(SignalGraphPassResult {
			signals: ctx.signals,
			elab_signals: ctx.elab_signals,
			pass_info: ctx.pass_info,
		});
		Ok(full_ctx)
	}
}
