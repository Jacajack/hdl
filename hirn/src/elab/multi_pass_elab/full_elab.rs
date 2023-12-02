use std::{
	collections::VecDeque,
	sync::{Arc, Mutex},
};

use log::info;

use crate::{
	design::{DesignHandle, EvalError, ModuleHandle, ModuleId},
	elab::{ElabAssumptions, ElabAssumptionsBase, ElabError, ElabMessage, ElabMessageKind, ElabReport, Elaborator},
};

use super::{
	comb_verif_pass,
	main_pass::{MainPass, MainPassConfig, MainPassResult},
	signal_usage_pass, ElabPass, ElabPassContext, ElabQueueItem,
};

pub(super) struct FullElabCtx {
	design: DesignHandle,
	module_id: ModuleId,
	report: ElabReport,
	assumptions: Arc<dyn ElabAssumptionsBase>,

	pub(super) main_pass_result: Option<MainPassResult>,
	pub(super) main_pass_config: MainPassConfig,
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
		&self.report
	}
}

pub struct PartialElabResult {
	main_pass_result: MainPassResult,
	report: ElabReport,
	assumptions: Arc<ElabAssumptions>,
	module_id: ModuleId,
}

impl PartialElabResult {
	pub fn report(&self) -> &ElabReport {
		&self.report
	}

	pub fn main_pass_result(&self) -> &MainPassResult {
		&self.main_pass_result
	}

	pub fn assumptions(&self) -> Arc<dyn ElabAssumptionsBase> {
		self.assumptions.clone()
	}

	pub fn module_id(&self) -> ModuleId {
		self.module_id
	}
}

#[derive(Default)]
pub struct FullElabResult {
	partial_results: Vec<PartialElabResult>,
}

impl FullElabResult {
	fn append(&mut self, other: PartialElabResult) {
		self.partial_results.push(other);
	}

	pub fn partial_results(&self) -> &[PartialElabResult] {
		&self.partial_results
	}

	pub fn main_result(&self) -> &PartialElabResult {
		&self.partial_results[0]
	}

	pub fn get_result(&self, module_id: ModuleId, assumptions: Arc<ElabAssumptions>) -> Option<&PartialElabResult> {
		self.partial_results
			.iter()
			.find(|r| r.module_id == module_id && r.assumptions == assumptions)
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
			if let Some(_partial_result) = result.get_result(item.module, item.assumptions.clone()) {
				info!(
					"Skipping module {:?} with assumptions {:?} because it was already elaborated",
					item.module, item.assumptions
				);
			}
			else {
				result.append(self.elab_module(item.module, item.assumptions)?);
			}
		}
		Ok(result)
	}

	/// Runs all elaboration passes on the specified module
	fn elab_module(&mut self, id: ModuleId, assumptions: Arc<ElabAssumptions>) -> Result<PartialElabResult, ElabError> {
		let mut ctx = FullElabCtx::new_context(self.design.clone(), id, assumptions.clone(), Default::default());

		let mut main_pass = MainPass {};
		ctx = main_pass.init(ctx)?;
		ctx = main_pass.run(ctx)?;

		let mut signal_usage_pass = signal_usage_pass::SignalUsagePass {};
		ctx = signal_usage_pass.init(ctx)?;
		ctx = signal_usage_pass.run(ctx)?;

		let mut comb_verif_pass = comb_verif_pass::CombVerifPass {};
		ctx = comb_verif_pass.init(ctx)?;
		ctx = comb_verif_pass.run(ctx)?;

		self.queue.extend(ctx.queued().into_iter());

		Ok(PartialElabResult {
			report: ctx.report,
			main_pass_result: ctx.main_pass_result.unwrap(),
			module_id: ctx.module_id,
			assumptions,
		})
	}
}

impl Elaborator<FullElabResult> for FullElaborator {
	fn elaborate(&mut self, id: ModuleId, assumptions: Arc<ElabAssumptions>) -> Result<FullElabResult, ElabError> {
		if assumptions.design().is_none() {
			return Err(EvalError::NoDesign.into());
		}
		info!("Starting full elab module {:?}", id);

		self.queue.push_back(ElabQueueItem::new(id, assumptions));
		self.run_queue()
	}
}
