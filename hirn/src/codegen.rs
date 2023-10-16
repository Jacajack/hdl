pub mod sv;

use crate::design::ModuleId;
use std::fmt;
use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum CodegenError {
	#[error(transparent)]
	FormatError(#[from] fmt::Error),

	#[error("Invalid module ID")]
	InvalidModuleId(ModuleId),
}

pub trait Codegen {
	fn emit_module(&mut self, w: &mut dyn fmt::Write, module: ModuleId) -> Result<(), CodegenError>;
}
