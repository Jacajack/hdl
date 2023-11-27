use log::{info, error};

use crate::elab::{
	ElabError, ElabMessageKind,
};

use super::{
	full_elab::{FullElabCacheHandle, FullElabCtx},
	ElabPass, main_pass::CombGraph, GeneratedSignalRef,
};

pub(super) struct CombVerifPass;


fn find_comb_loop_global(g: &CombGraph) -> Vec<ElabMessageKind> {
	let mut messages = vec![];

	// Split graph into strongly connected components
	let scc = petgraph::algo::tarjan_scc(g);
	assert!(!scc.is_empty(), "existence of cycles implies non-empty SCC list");
	info!("Searching for comb loops in {} SCCs", scc.len());

	for component in scc {
		assert!(!component.is_empty());
		if component.len() < 2 {
			continue;
		}

		// Note: the signals are not necessarily in topological order
		messages.push(ElabMessageKind::CombLoop {
			signals: component.clone(),
		});
	}

	messages
}

fn check_comb_loops(g: &CombGraph) -> Vec<ElabMessageKind> {
	if petgraph::algo::is_cyclic_directed(g) {
		error!("Comb loop check failed - found loops");
		let msgs = find_comb_loop_global(g);
		assert!(!msgs.is_empty(), "petgraph says there are cycles, but we didn't find any");
		msgs
	}
	else {
		info!("Comb loop check passed - no loops found");
		vec![]
	}
}

fn check_interface_loops(g: &CombGraph) -> Vec<ElabMessageKind> {
	vec![]
}

fn check_comb_graph(g: &CombGraph) -> Vec<ElabMessageKind> {
	let msg = check_comb_loops(g);
	if !msg.is_empty() {
		return msg;
	}

	check_interface_loops(g)
}

impl ElabPass<FullElabCtx, FullElabCacheHandle> for CombVerifPass {
	fn name(&self) -> &'static str {
		"SignalUsagePass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running signal usage check pass...");

		let messages = check_comb_graph(full_ctx.main_pass_result.as_ref().unwrap().comb_graph());
		for msg in messages {
			full_ctx.add_message(msg);
		}

		Ok(full_ctx)
	}
}
