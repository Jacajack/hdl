use std::collections::{HashSet, HashMap};
use std::sync::Arc;
use log::info;

use crate::design::{ScopeHandle, ConditionalScope, RangeScope, Evaluates};
use crate::elab::{ElabSignal, ElabAssumptionsBase};
use crate::{elab::{ElabError, ElabAssumptions}, design::{ScopeId, SignalId, NarrowEval}};
use crate::elab::GenericVar;

use super::{full_elab::{FullElabCtx, FullElabCacheHandle}, ElabPass};

#[derive(Default)]
pub(super) struct GenericResolvePassCtx {
	signals: HashMap<(SignalId, usize), ElabSignal>,
	assumptions: HashMap<(SignalId, Vec<GenericVar>, usize), GenericVar>,
}

impl GenericResolvePassCtx {
	fn analyze_resolved_scope(&mut self, scope: ScopeHandle, assumptions: Arc<dyn ElabAssumptionsBase>) {
		for sig_id in scope.signals() {
			let sig = scope.design().get_signal(sig_id).unwrap();
			let width = sig.width().eval(&assumptions).unwrap();
			info!("Found signal {} with width {:?}", sig.name(), width);
		}
	}

	fn analyze_unconditional_scope(&mut self, scope: ScopeHandle, assumptions: Arc<dyn ElabAssumptionsBase>) {
		
	}

	fn analyze_if_scope(&mut self, cond_scope: &ConditionalScope, assumptions: Arc<dyn ElabAssumptionsBase>) {
		// cond_scope.condition.narrow_eval(ctx);
	}

	fn analyze_for_scope(&mut self, range_scope: &RangeScope, assumptions: Arc<dyn ElabAssumptionsBase>) {
		
	}
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

		ctx.analyze_resolved_scope(module.scope(), full_ctx.assumptions());

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
