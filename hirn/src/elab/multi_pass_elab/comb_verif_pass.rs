use std::collections::HashMap;

use log::{info, error, warn};

use crate::{elab::{
	ElabError, ElabMessageKind,
}, design::{ModuleHandle, SignalDirection}};

use super::{
	full_elab::{FullElabCacheHandle, FullElabCtx},
	ElabPass, main_pass::CombGraph, GeneratedSignalRef, GeneratedSignalId, GeneratedSignal,
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

fn check_interface_loops(g: &CombGraph, sigs: &HashMap<GeneratedSignalId, GeneratedSignal>) -> Vec<ElabMessageKind> {
	info!("Checking interface loops...");
	let mut input_refs = vec![];
	let mut output_refs = vec![];

	for (id, sig) in sigs {
		let mut refs = vec![];
		if sig.is_array() {
			for i in 0..sig.total_fields() {
				refs.push(GeneratedSignalRef::new(*id, Some(i as u32)));
			}
		}
		else {
			refs.push(GeneratedSignalRef::new(*id, None))
		}
		
		match sig.direction() {
			None => {},
			Some(SignalDirection::Tristate) => unreachable!("tristate ZBBd"),
			Some(SignalDirection::Input) => input_refs.extend(refs),
			Some(SignalDirection::Output) => output_refs.extend(refs),
		}
	}

	let total_possible_paths = input_refs.len() * output_refs.len();
	if total_possible_paths > 1000 { // FIXME configuration
		info!("Skipping interface check - too many paths to check ({} > 1000)", total_possible_paths);
		return vec![ElabMessageKind::ComplexInterface { num_inputs: input_refs.len(), num_outputs: output_refs.len() }]
	}

	info!("Checking {} possible interface-passtghrough paths", total_possible_paths);

	let mut messages = vec![];
	for input_ref in &input_refs {
		for output_ref in &output_refs {
			let path_exists = petgraph::algo::has_path_connecting(g, *output_ref, *input_ref, None);
			if path_exists {
				warn!("Interface comb path from {:?} to {:?}", input_ref, output_ref);
				
				// TODO Improvement area - report the entire path
				messages.push(ElabMessageKind::CombInterfaceLoop {
					from: Box::new(*input_ref),
					to: Box::new(*output_ref) 
				});
			}
		}
	}

	messages
}

fn check_comb_graph(g: &CombGraph, sigs: &HashMap<GeneratedSignalId, GeneratedSignal>) -> Vec<ElabMessageKind> {
	let msg = check_comb_loops(g);
	if !msg.is_empty() {
		return msg;
	}

	check_interface_loops(g, sigs)
}

impl ElabPass<FullElabCtx, FullElabCacheHandle> for CombVerifPass {
	fn name(&self) -> &'static str {
		"SignalUsagePass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running signal usage check pass...");

		let messages = check_comb_graph(
			full_ctx.main_pass_result.as_ref().unwrap().comb_graph(),
			full_ctx.main_pass_result.as_ref().unwrap().signals()
		);

		for msg in messages {
			full_ctx.add_message(msg);
		}

		Ok(full_ctx)
	}
}
