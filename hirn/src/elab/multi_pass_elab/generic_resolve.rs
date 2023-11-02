use std::collections::{HashSet, HashMap};

use log::info;

use crate::{elab::ElabError, design::{ScopeId, SignalId, NarrowEval}};

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

		for asmt in module.scope().assignments() {
			let lhs_bits = asmt.lhs.try_drive_bits().expect("undrivable lhs");
			let rhs_slices = asmt.rhs.get_used_slice_ranges();

			// info!("Assignment to {:?}", lhs_bits);
			// info!("Using slices {:?}", rhs_slices);
			for slice in rhs_slices {
				let sig = full_ctx.design().get_signal(slice.signal()).unwrap();
				match slice.lsb_msb() {
					Some((lsb, msb)) => {
						let lsb_val = lsb.const_narrow_eval().unwrap();
						let msb_val = msb.const_narrow_eval().unwrap();
						info!("Marking {}[{}:{}] as used", sig.name(), lsb_val, msb_val);
					}
					None => info!("Marking {} as used", sig.name())
				}
			}
		}

		full_ctx.generic_resolve_ctx = Some(ctx);
		Ok(full_ctx)
	}
}
