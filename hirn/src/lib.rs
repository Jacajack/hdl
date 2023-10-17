pub mod codegen;
pub mod design;

use thiserror::Error;
use design::DesignError;
use codegen::CodegenError;

#[derive(Clone, Error, Debug)]
pub enum HirnError {
	#[error(transparent)]
	DesignError(#[from] DesignError),

	#[error(transparent)]
	CodegenError(#[from] CodegenError),
}
