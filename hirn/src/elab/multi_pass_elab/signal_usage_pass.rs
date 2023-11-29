use log::info;

use crate::elab::{ElabError, ElabMessageKind};

use super::{
	full_elab::{FullElabCacheHandle, FullElabCtx},
	ElabPass,
};

pub(super) struct SignalUsagePass;

fn get_messages(ctx: &FullElabCtx) -> Vec<ElabMessageKind> {
	let sig_graph = ctx.main_pass_result.as_ref().unwrap();
	let mut messages = Vec::new();

	for (sig_ref, elab_sig) in sig_graph.elab_signals() {
		if !elab_sig.is_fully_driven() {
			if elab_sig.is_read() {
				messages.push(ElabMessageKind::SignalNotDrivenAndUsed {
					signal: Box::new(sig_ref.clone()),
					elab: Box::new((*elab_sig).clone()),
				});
			}
			else {
				messages.push(ElabMessageKind::SignalNotDriven {
					signal: Box::new(sig_ref.clone()),
					elab: Box::new((*elab_sig).clone()),
				});
			}
		}

		if !elab_sig.is_fully_read() {
			// warn!("Signal {} is not fully read: {:?}", sig_name, elab_sig.unread_summary());
			messages.push(ElabMessageKind::SignalUnused {
				signal: Box::new(sig_ref.clone()),
				elab: Box::new((*elab_sig).clone()),
			});
		}

		if elab_sig.has_conflicts() {
			// error!("Signal {} has conflicts: {:?}", sig_name, elab_sig.conflict_summary());
			messages.push(ElabMessageKind::SignalConflict {
				signal: Box::new(sig_ref.clone()),
				elab: Box::new((*elab_sig).clone()),
			});
		}
	}

	messages
}

impl ElabPass<FullElabCtx, FullElabCacheHandle> for SignalUsagePass {
	fn name(&self) -> &'static str {
		"SignalUsagePass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running signal usage check pass...");

		let messages = get_messages(&full_ctx);
		for msg in messages {
			full_ctx.add_message(msg);
		}

		Ok(full_ctx)
	}
}
