use std::sync::Arc;

use crate::design::ModuleId;

use super::{ElabAssumptionsBase, ElabError};

/// Trait which must be implemented by all elaborators
pub trait Elaborator<R> {
	fn elaborate(&mut self, id: ModuleId, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<R, ElabError>;
}
