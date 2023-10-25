mod generic_resolve;
mod test_pass;
mod full_elab;

pub use full_elab::FullElaborator;

use std::collections::VecDeque;

use crate::design::ModuleId;

use super::{ElabError, Elaborator, ElabAssumptionsBase, ElabReport};


pub trait ElabPassContext<T> {
	fn cache(&self) -> T;
	fn from_cache(cache: T) -> Self;
	fn queued(&self) -> Vec<ElabQueueItem>;
}

pub trait ElabPass<Ctx, Cache> 
where
	Ctx: Default + ElabPassContext<Cache>
{
	fn run(&mut self, c: Ctx) -> Result<Ctx, ElabError>;
}

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

	pub fn add_pass(&mut self, pass: Box<dyn ElabPass<Ctx, Cache>>) {
		self.passes.push(pass);
	}

	fn run_queue(&mut self) -> Result<ElabReport, ElabError> {
		let mut report = ElabReport::default();
		while let Some(item) = self.queue.pop_front() {
			report.extend(self.elab_module(item.module, item.assumptions)?);
		}
		Ok(report)
	}

	fn elab_module(&mut self, _id: ModuleId, _assumptions: Box<dyn ElabAssumptionsBase>) -> Result<ElabReport, ElabError> {
		todo!();
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

