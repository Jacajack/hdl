use crate::{elab::{ElabError, Elaborator, ElabReport, ElabAssumptionsBase}, design::{ModuleId, DesignHandle}};

use super::{ElabPassContext, MultiPassElaborator, ElabQueueItem, test_pass::TestPass};

pub(super) struct FullElabCtx {

}

impl ElabPassContext<FullElabCache> for FullElabCtx {
	fn new_context(design: DesignHandle, module_id: ModuleId, assumptions: Box<dyn ElabAssumptionsBase>) -> Self {todo!();}

	fn cache(&self) -> FullElabCache {
		todo!();
	}

	fn from_cache(_cache: FullElabCache) -> Self {
		todo!();
	}

	fn queued(&self) -> Vec<ElabQueueItem> {
		vec![]
	}

	fn report(&self) -> ElabReport {
		todo!();
	}
}

impl Default for FullElabCtx {
	fn default() -> Self {
		todo!();
	}
}

pub(super) struct FullElabCache {

}


pub struct FullElaborator {
	elaborator: MultiPassElaborator<FullElabCtx, FullElabCache>,
}

impl FullElaborator {
	pub fn new() -> Self {
		let mut elaborator = MultiPassElaborator::new();
		elaborator.add_pass(Box::new(TestPass{}));
		Self {
			elaborator,
		}
	}
}

impl Elaborator for FullElaborator {
	fn elaborate(&mut self, id: ModuleId, assumptions: Box<dyn super::ElabAssumptionsBase>) -> Result<ElabReport, ElabError> {
		self.elaborator.elaborate(id, assumptions)
	}
}
