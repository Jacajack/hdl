use std::sync::{Arc, Mutex};

use log::info;

use crate::elab::ElabError;

use super::{
	full_elab::{FullElabCacheHandle, FullElabCtx},
	ElabPass,
};

pub(super) struct TestPass;

impl ElabPass<FullElabCtx, FullElabCacheHandle> for TestPass {
	fn name(&self) -> &'static str {
		"TestPass"
	}

	fn run(&mut self, ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running test elaboration pass...");
		Ok(ctx)
	}
}
