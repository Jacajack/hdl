use std::sync::{Arc, Mutex};

use log::info;

use crate::{
	design::{DesignHandle, EvalError, ModuleHandle, ModuleId},
	elab::{
		ElabAssumptionsBase, ElabError, ElabMessage, ElabMessageKind, ElabReport, Elaborator,
	},
};

use super::{
	main_pass::{MainPass, MainPassConfig, MainPassResult},
	signal_usage_pass::SignalUsagePass,
	ElabPassContext, ElabQueueItem, MultiPassElaborator,
};

pub(super) struct FullElabCtx {
	design: DesignHandle,
	module_id: ModuleId,
	report: ElabReport,
	assumptions: Arc<dyn ElabAssumptionsBase>,

	pub(super) sig_graph_result: Option<MainPassResult>,
	pub(super) sig_graph_config: MainPassConfig,
}

impl FullElabCtx {
	pub(super) fn add_message(&mut self, kind: ElabMessageKind) {
		self.report
			.add_message(ElabMessage::new(kind, self.module_id, self.assumptions.clone()));
	}

	pub fn module_handle(&self) -> ModuleHandle {
		self.design
			.get_module_handle(self.module_id)
			.expect("elaborated module not in design")
	}

	pub fn design(&self) -> &DesignHandle {
		&self.design
	}

	pub fn assumptions(&self) -> Arc<dyn ElabAssumptionsBase> {
		self.assumptions.clone()
	}
}

#[derive(Default)]
pub(super) struct FullElabCache {}
pub(super) type FullElabCacheHandle = Arc<Mutex<FullElabCache>>;

impl ElabPassContext<FullElabCacheHandle> for FullElabCtx {
	fn new_context(
		design: DesignHandle,
		module_id: ModuleId,
		assumptions: Arc<dyn ElabAssumptionsBase>,
		_cache: FullElabCacheHandle,
	) -> Self {
		assumptions.design().expect("assumptions must have a design");
		Self {
			design,
			module_id,
			assumptions,
			report: ElabReport::default(),
			sig_graph_config: MainPassConfig::default(),
			sig_graph_result: None,
		}
	}

	fn queued(&self) -> Vec<ElabQueueItem> {
		if let Some(result) = &self.sig_graph_result {
			result
				.queued_modules()
				.iter()
				.map(|(module, assumptions)| ElabQueueItem {
					module: module.id(),
					assumptions: assumptions.clone(),
				})
				.collect()
		}
		else {
			vec![]
		}
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
		elaborator.add_pass(Box::new(MainPass {}));
		elaborator.add_pass(Box::new(SignalUsagePass {}));
		Self { elaborator }
	}
}

impl Elaborator<ElabReport> for FullElaborator {
	fn elaborate(
		&mut self,
		id: ModuleId,
		assumptions: Arc<dyn super::ElabAssumptionsBase>,
	) -> Result<ElabReport, ElabError> {
		if assumptions.design().is_none() {
			return Err(EvalError::NoDesign.into());
		}
		info!("Starting full elab module {:?}", id);
		self.elaborator.elaborate(id, assumptions)
	}
}
