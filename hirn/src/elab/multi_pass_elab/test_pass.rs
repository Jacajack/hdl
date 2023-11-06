use log::info;

use crate::elab::{ElabError, ElabMessageKind};

use super::{
	full_elab::{FullElabCacheHandle, FullElabCtx},
	ElabPass,
};

pub(super) struct TestPass;

impl ElabPass<FullElabCtx, FullElabCacheHandle> for TestPass {
	fn name(&self) -> &'static str {
		"TestPass"
	}

	fn run(&mut self, mut ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running test elaboration pass...");

		ctx.add_message(ElabMessageKind::Notice("asdf".into()));
		Ok(ctx)
	}
}
