pub mod sv_codegen;
pub mod indenter;

use thiserror::Error;
use crate::ModuleId;
use std::fmt;

#[derive(Error, Debug)]
pub enum CodegenError {
	#[error(transparent)]
	FormatError(fmt::Error),
}

trait Codegen {
	fn emit_module(&mut self, stream: &mut fmt::Formatter<'_>, module: ModuleId) -> Result<(), CodegenError>;
}