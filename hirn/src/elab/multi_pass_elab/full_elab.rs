use std::{sync::{Arc, Mutex}, collections::VecDeque};

use log::info;

use crate::{
	design::{DesignHandle, EvalError, ModuleHandle, ModuleId},
	elab::{
		ElabAssumptionsBase, ElabError, ElabMessage, ElabMessageKind, ElabReport, Elaborator,
	},
};

use super::{main_pass::{MainPassConfig, MainPassResult, MainPass}, ElabPassContext, ElabQueueItem, signal_usage_pass, comb_verif_pass, ElabPass};

pub(super) struct FullElabCtx {
	design: DesignHandle,
	module_id: ModuleId,
	result: FullElabResult,
	assumptions: Arc<dyn ElabAssumptionsBase>,

	pub(super) main_pass_result: Option<MainPassResult>,
	pub(super) main_pass_config: MainPassConfig,
}

impl FullElabCtx {
	pub(super) fn add_message(&mut self, kind: ElabMessageKind) {
		self.result.report_mut()
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
			result: FullElabResult::default(),
			main_pass_config: MainPassConfig::default(),
			main_pass_result: None,
		}
	}

	fn queued(&self) -> Vec<ElabQueueItem> {
		if let Some(result) = &self.main_pass_result {
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
		self.result.report()
	}
}

#[derive(Clone, Debug, Default)]
pub struct FullElabResult {
	report: ElabReport,
}

impl FullElabResult {
	fn combine(mut self, other: FullElabResult) -> Self {
		self.report.extend(&other.report);
		self
	}

	pub fn report(&self) -> &ElabReport {
		&self.report
	}

	fn report_mut(&mut self) -> &mut ElabReport {
		&mut self.report
	}
}

/// Multi-pass elaborator with all passes
pub struct FullElaborator {
	design: DesignHandle,
	queue: VecDeque<ElabQueueItem>,
}

impl FullElaborator {
	pub fn new(design: DesignHandle) -> Self {
		Self {
			design,
			queue: VecDeque::new(),
		}
	}

	/// Runs elaboration on all modules in the queue
	fn run_queue(&mut self) -> Result<FullElabResult, ElabError> {
		let mut result = FullElabResult::default();
		while let Some(item) = self.queue.pop_front() {
			result = result.combine(self.elab_module(item.module, item.assumptions)?);
		}
		Ok(result)
	}

	/// Runs all elaboration passes on the specified module
	fn elab_module(
		&mut self,
		id: ModuleId,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<FullElabResult, ElabError> {
		let mut ctx = FullElabCtx::new_context(self.design.clone(), id, assumptions, Default::default());

		let mut main_pass = MainPass{};
		ctx = main_pass.init(ctx)?;
		ctx = main_pass.run(ctx)?;

		let mut signal_usage_pass = signal_usage_pass::SignalUsagePass{};
		ctx = signal_usage_pass.init(ctx)?;
		ctx = signal_usage_pass.run(ctx)?;

		let mut comb_verif_pass = comb_verif_pass::CombVerifPass{};
		ctx = comb_verif_pass.init(ctx)?;
		ctx = comb_verif_pass.run(ctx)?;

		self.queue.extend(ctx.queued().into_iter());
		Ok(ctx.result)
	}
}

impl Elaborator<FullElabResult> for FullElaborator {
	fn elaborate(
		&mut self,
		id: ModuleId,
		assumptions: Arc<dyn super::ElabAssumptionsBase>,
	) -> Result<FullElabResult, ElabError> {
		if assumptions.design().is_none() {
			return Err(EvalError::NoDesign.into());
		}
		info!("Starting full elab module {:?}", id);

		self.queue.push_back(ElabQueueItem::new(id, assumptions));
		self.run_queue()
	}
}
