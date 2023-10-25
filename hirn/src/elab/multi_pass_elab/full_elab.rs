use std::sync::{Arc, Mutex};

use crate::{
	design::{DesignHandle, ModuleId},
	elab::{ElabAssumptionsBase, ElabError, ElabReport, Elaborator},
};

use super::{test_pass::TestPass, ElabPassContext, ElabQueueItem, MultiPassElaborator};

pub(super) struct FullElabCtx {
	design: DesignHandle,
	module_id: ModuleId,
	report: ElabReport,
	queued: Vec<ElabQueueItem>,
	assumptions: Box<dyn ElabAssumptionsBase>,
}

#[derive(Default)]
pub(super) struct FullElabCache {}

pub(super) type FullElabCacheHandle = Arc<Mutex<FullElabCache>>;

impl ElabPassContext<FullElabCacheHandle> for FullElabCtx {
	fn new_context(
		design: DesignHandle,
		module_id: ModuleId,
		assumptions: Box<dyn ElabAssumptionsBase>,
		cache: FullElabCacheHandle,
	) -> Self {
		Self {
			design,
			module_id,
			assumptions,
			queued: Vec::new(),
			report: ElabReport::default(),
		}
	}

	fn queued(&self) -> Vec<ElabQueueItem> {
		self.queued.clone()
	}

	fn report(&self) -> &ElabReport {
		&self.report
	}
}

/// Multi-pass elaborator with all passes
pub struct FullElaborator {
	elaborator: MultiPassElaborator<FullElabCtx, Arc<Mutex<FullElabCache>>>,
}

impl FullElaborator {
	/// Create a new FullElaborator and add all passes
	pub fn new(design: DesignHandle) -> Self {
		let mut elaborator = MultiPassElaborator::new(design);
		elaborator.add_pass(Box::new(TestPass {}));
		Self { elaborator }
	}
}

impl Elaborator for FullElaborator {
	fn elaborate(
		&mut self,
		id: ModuleId,
		assumptions: Box<dyn super::ElabAssumptionsBase>,
	) -> Result<ElabReport, ElabError> {
		self.elaborator.elaborate(id, assumptions)
	}
}
