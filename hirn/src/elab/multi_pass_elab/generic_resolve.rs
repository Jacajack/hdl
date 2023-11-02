use std::collections::{HashSet, HashMap};

use log::info;

use crate::{elab::ElabError, design::{ScopeId, SignalId}};

use super::{full_elab::{FullElabCtx, FullElabCacheHandle}, ElabPass};

#[derive(Default)]
pub(super) struct GenericResolvePassCtx {
	scope_condition_depends: HashMap<ScopeId, HashSet<SignalId>>,
	scope_depends: HashMap<ScopeId, HashSet<SignalId>>,
	scope_results: HashMap<ScopeId, HashSet<SignalId>>,
}

pub(super) struct GenericResolvePass;

impl ElabPass<FullElabCtx, FullElabCacheHandle> for GenericResolvePass {
	fn name(&self) -> &'static str {
		"GenericResolvePass"
	}

	fn run(&mut self, mut full_ctx: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		info!("Running generic resolution pass...");
		let mut ctx = GenericResolvePassCtx::default();
		let module = full_ctx.module_handle();

		full_ctx.generic_resolve_ctx = Some(ctx);
		Ok(full_ctx)
	}
}
