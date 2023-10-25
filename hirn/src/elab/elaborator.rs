use std::collections::HashMap;

use crate::design::{ModuleId, ModuleHandle};

use super::{ElabError, ElabMessage, GenericVar, ElabAssumptionsBase, ElabReport};

pub trait Elaborator {
	fn elaborate(&mut self, id: ModuleId, assumptions: Box<dyn ElabAssumptionsBase>) -> Result<ElabReport, ElabError>;

	fn elaborate_bind_interface(&mut self, _module: ModuleHandle, _bindings: &HashMap<String, GenericVar>) -> Result<Vec<ElabMessage>, ElabError> {
		todo!();
	}
}