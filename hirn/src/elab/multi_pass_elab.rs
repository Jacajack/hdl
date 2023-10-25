mod generic_resolve;
mod test_pass;
mod full_elab;

pub use full_elab::FullElaborator;
use log::info;

use std::collections::VecDeque;

use crate::design::{ModuleId, DesignHandle};

use super::{ElabError, Elaborator, ElabAssumptionsBase, ElabReport};

/// Elaboration pass context (for MultiPassElaborator)
pub trait ElabPassContext<T> {
	fn new_context(design: DesignHandle, module_id: ModuleId, assumptions: Box<dyn ElabAssumptionsBase>) -> Self;
	fn cache(&self) -> T; // TODO remove
	fn from_cache(cache: T) -> Self; // TODO remove
	fn queued(&self) -> Vec<ElabQueueItem>;
	fn report(&self) -> ElabReport;
	// fn cache_mut(&mut self) -> &mut T;
}

/// Elaboration pass trait (for MultiPassElaborator)
pub trait ElabPass<Ctx, Cache> 
where
	Ctx: Default + ElabPassContext<Cache>
{
	/// Returns name of the elaboration pass
	fn name(&self) -> &'static str;

	/// Runs the elaboration pass on the specified context
	fn run(&mut self, c: Ctx) -> Result<Ctx, ElabError>;
}

/// Item in the elaboration queue (module + assumptions)
/// for (MultiPassElaborator)
pub struct ElabQueueItem {
	module: ModuleId,
	assumptions: Box<dyn ElabAssumptionsBase>,
}

impl ElabQueueItem {
	pub fn new(module: ModuleId, assumptions: Box<dyn ElabAssumptionsBase>) -> Self {
		Self {
			module,
			assumptions,
		}
	}
}

/// Multi-pass design elaborator 
/// Ultimately we'll want a multi-threaded version of that.
pub struct MultiPassElaborator<Ctx, Cache> 
where 
	Ctx: Default + ElabPassContext<Cache>,
{
	passes: Vec<Box<dyn ElabPass<Ctx, Cache>>>,
	queue: VecDeque<ElabQueueItem>,
}

impl<Ctx, Cache> MultiPassElaborator<Ctx, Cache>
where 
	Ctx: Default + ElabPassContext<Cache>
{
	pub fn new() -> Self {
		Self {
			passes: vec![],
			queue: VecDeque::new(),
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
			report.extend(self.elab_module(item.module, item.assumptions)?);
		}
		Ok(report)
	}

	/// Runs all elaboration passes on the specified module
	fn elab_module(&mut self, id: ModuleId, assumptions: Box<dyn ElabAssumptionsBase>) -> Result<ElabReport, ElabError> {
		let mut ctx = Ctx::new_context(todo!(), id, assumptions);

		for pass in &self.passes {
			ctx = pass.run(ctx)?;
		}

		self.queue.extend(ctx.queued().into_iter());
		Ok(ctx.report())
	}
}

impl<Ctx, Cache> Elaborator for MultiPassElaborator<Ctx, Cache> 
where 
	Ctx: Default + ElabPassContext<Cache>,
{
	fn elaborate(&mut self, id: ModuleId, assumptions: Box<dyn ElabAssumptionsBase>) -> Result<ElabReport, ElabError> {
		self.queue.push_back(ElabQueueItem{
			module: id,
			assumptions,
		});

		self.run_queue()
	}
}

