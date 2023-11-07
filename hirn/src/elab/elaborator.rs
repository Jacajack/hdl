use std::{collections::HashMap, sync::Arc};

use crate::design::{ModuleHandle, ModuleId};

use super::{ElabAssumptionsBase, ElabError, ElabMessage, ElabReport, GenericVar};

/// Trait which must be implemented by all elaborators
pub trait Elaborator {
	fn elaborate(&mut self, id: ModuleId, assumptions: Arc<dyn ElabAssumptionsBase>) -> Result<ElabReport, ElabError>;

	fn elaborate_bind_interface(
		&mut self,
		_module: ModuleHandle,
		_bindings: &HashMap<String, GenericVar>,
	) -> Result<Vec<ElabMessage>, ElabError> {
		todo!();
	}
}
