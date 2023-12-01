pub mod sv;

use crate::design::{EvalError, ModuleId};
use std::fmt;
use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum CodegenError {
	#[error(transparent)]
	FormatError(#[from] fmt::Error),

	#[error(transparent)]
	EvalError(#[from] Box<EvalError>),

	#[error("Invalid module ID")]
	InvalidModuleId(ModuleId),
}

impl From<EvalError> for CodegenError {
	fn from(e: EvalError) -> Self {
		Self::EvalError(Box::new(e))
	}
}

pub trait Codegen {
	fn emit_module(&mut self, module: ModuleId) -> Result<(), CodegenError>;
}
