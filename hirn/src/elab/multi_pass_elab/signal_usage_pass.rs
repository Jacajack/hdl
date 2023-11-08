use log::{info, warn, error};

use crate::elab::{ElabError, ElabMessageKind};

use super::{ElabPass, full_elab::{FullElabCtx, FullElabCacheHandle}};

pub(super) struct SignalUsagePass;

impl ElabPass<FullElabCtx, FullElabCacheHandle> for SignalUsagePass {
	fn name(&self) -> &'static str {
		"SignalUsagePass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running signal usage check pass...");

		let sig_graph = full_ctx.sig_graph_result.as_ref().unwrap();

		for (sig_ref, elab_sig) in sig_graph.elab_signals() {
			if !elab_sig.is_fully_driven() {
				error!("Signal {:?} is not fully driven", sig_ref);
			}

			if !elab_sig.is_fully_read() {
				warn!("Signal {:?} is not fully read", sig_ref);
			}

			if elab_sig.has_conflicts() {
				error!("Signal {:?} has conflicts", sig_ref);
			}
		}

		Ok(full_ctx)
	}
}
