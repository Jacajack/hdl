mod comb_verif_pass;
mod full_elab;
mod main_pass;
mod signal_usage_pass;

pub use full_elab::{FullElaborator, PartialElabResult, FullElabResult};
pub use main_pass::{GeneratedSignal, GeneratedSignalId, GeneratedSignalRef, ScopePassId, ScopePassInfo};

use std::sync::Arc;

use crate::design::{DesignHandle, ModuleId};

use super::{ElabAssumptionsBase, ElabError, ElabReport, ElabAssumptions};

/// Item in the elaboration queue (module + assumptions)
/// for (MultiPassElaborator)
#[derive(Clone, Debug)]
pub struct ElabQueueItem {
	module: ModuleId,
	assumptions: Arc<ElabAssumptions>,
}

impl ElabQueueItem {
	pub fn new(module: ModuleId, assumptions: Arc<ElabAssumptions>) -> Self {
		Self { module, assumptions }
	}
}

/// Elaboration pass context (for MultiPassElaborator)
pub trait ElabPassContext<T> {
	fn new_context(
		design: DesignHandle,
		module_id: ModuleId,
		assumptions: Arc<dyn ElabAssumptionsBase>,
		cache: T,
	) -> Self;
	fn queued(&self) -> Vec<ElabQueueItem>;
	fn report(&self) -> &ElabReport;
}

/// Elaboration pass trait (for MultiPassElaborator)
pub trait ElabPass<Ctx, Cache>
where
	Ctx: ElabPassContext<Cache>,
{
	/// Returns name of the elaboration pass
	fn name(&self) -> &'static str;

	/// Ran before run() to initialize the pass
	fn init(&mut self, c: Ctx) -> Result<Ctx, ElabError> {
		Ok(c)
	}

	/// Runs the elaboration pass on the specified context
	fn run(&mut self, c: Ctx) -> Result<Ctx, ElabError>;
}
