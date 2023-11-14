mod full_elab;
mod signal_graph_pass;
mod signal_usage_pass;

pub use full_elab::FullElaborator;
use log::info;

use std::{collections::VecDeque, sync::Arc};

use crate::design::{DesignHandle, ModuleId};

use super::{ElabAssumptionsBase, ElabError, ElabReport, Elaborator};

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

/// Item in the elaboration queue (module + assumptions)
/// for (MultiPassElaborator)
#[derive(Clone, Debug)]
pub struct ElabQueueItem {
	module: ModuleId,
	assumptions: Arc<dyn ElabAssumptionsBase>,
}

impl ElabQueueItem {
	pub fn new(module: ModuleId, assumptions: Arc<dyn ElabAssumptionsBase>) -> Self {
		Self { module, assumptions }
	}
}

/// Multi-pass design elaborator
/// Ultimately we'll want a multi-threaded version of that.
/// This isn't possible with DesignHandle though.
pub struct MultiPassElaborator<Ctx, Cache>
where
	Ctx: ElabPassContext<Cache>,
{
	design: DesignHandle,
	passes: Vec<Box<dyn ElabPass<Ctx, Cache>>>,
	queue: VecDeque<ElabQueueItem>,
	cache: Cache,
}

impl<Ctx, Cache> MultiPassElaborator<Ctx, Cache>
where
	Ctx: ElabPassContext<Cache>,
	Cache: Default + Clone,
{
	pub fn new(design: DesignHandle) -> Self {
		Self {
			design,
			passes: vec![],
			queue: VecDeque::new(),
			cache: Cache::default(),
		}
	}

	/// Adds a new pass to the elaborator
	pub fn add_pass(&mut self, pass: Box<dyn ElabPass<Ctx, Cache>>) {
		info!("Registering elaboration pass: {}", pass.name());
		self.passes.push(pass);
	}

	/// Runs elaboration on all modules in the queue
	fn run_queue(&mut self) -> Result<ElabReport, ElabError> {
		let mut report = ElabReport::default();
		while let Some(item) = self.queue.pop_front() {
			report.extend(&self.elab_module(item.module, item.assumptions)?);
		}
		Ok(report)
	}

	/// Runs all elaboration passes on the specified module
	fn elab_module(
		&mut self,
		id: ModuleId,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<ElabReport, ElabError> {
		let mut ctx = Ctx::new_context(self.design.clone(), id, assumptions, self.cache.clone());

		for pass in &mut self.passes {
			ctx = pass.init(ctx)?;
			ctx = pass.run(ctx)?;
		}

		self.queue.extend(ctx.queued().into_iter());
		Ok(ctx.report().clone())
	}
}

/// Elaborator trait implementation for MultiPassElaborator
/// adds a module to the queue and elaborates the entire queue
impl<Ctx, Cache> Elaborator for MultiPassElaborator<Ctx, Cache>
where
	Ctx: ElabPassContext<Cache>,
	Cache: Default + Clone,
{
	fn elaborate(&mut self, id: ModuleId, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<ElabReport, ElabError> {
		self.queue.push_back(ElabQueueItem::new(id, assumptions));

		self.run_queue()
	}
}
