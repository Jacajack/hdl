use log::info;

use crate::elab::{
	ElabError, ElabMessageKind,
};

use super::{
	full_elab::{FullElabCacheHandle, FullElabCtx},
	ElabPass, main_pass::CombGraph,
};

pub(super) struct CombVerifPass;

fn check_comb_graph(g: &CombGraph) -> Vec<ElabMessageKind> {
	// todo!();
	vec![]
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
