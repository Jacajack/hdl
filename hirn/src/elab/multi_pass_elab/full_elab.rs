use std::sync::{Arc, Mutex};

use crate::{
	design::{DesignHandle, ModuleId, ModuleHandle},
	elab::{ElabAssumptionsBase, ElabError, ElabReport, Elaborator, ElabMessage, ElabMessageKind, SeverityPolicy, DefaultSeverityPolicy},
};

use super::{ElabPassContext, ElabQueueItem, MultiPassElaborator, signal_graph_pass::{SignalGraphPass, SignalGraphPassResult, SignalGraphPassConfig}, signal_usage_pass::SignalUsagePass};

pub(super) struct FullElabCtx {
	design: DesignHandle,
	module_id: ModuleId,
	report: ElabReport,
	queued: Vec<ElabQueueItem>,
	assumptions: Arc<dyn ElabAssumptionsBase>,
	severity_policy: Box<dyn SeverityPolicy>,

	pub(super) sig_graph_result: Option<SignalGraphPassResult>,
	pub(super) sig_graph_config: SignalGraphPassConfig,
}

impl FullElabCtx {
	pub(super) fn add_message(&mut self, kind: ElabMessageKind) {
		self.report.add_message(ElabMessage::new(
			kind,
			self.module_id,
			self.assumptions.clone(),
			Some(&*self.severity_policy),
		));
	}

	pub fn module_handle(&self) -> ModuleHandle {
		self.design.get_module_handle(self.module_id).expect("elaborated module not in design")
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
		Self {
			design,
			module_id,
			assumptions,
			queued: Vec::new(),
			report: ElabReport::default(),
			severity_policy: Box::new(DefaultSeverityPolicy),
			sig_graph_config: SignalGraphPassConfig::default(),
			sig_graph_result: None,
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
		elaborator.add_pass(Box::new(SignalGraphPass {}));
		elaborator.add_pass(Box::new(SignalUsagePass {}));
		Self { elaborator }
	}
}

impl Elaborator for FullElaborator {
	fn elaborate(
		&mut self,
		id: ModuleId,
		assumptions: Arc<dyn super::ElabAssumptionsBase>,
	) -> Result<ElabReport, ElabError> {
		self.elaborator.elaborate(id, assumptions)
	}
}
