pub mod codegen;
pub mod design;

use codegen::CodegenError;
use design::DesignError;
use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum HirnError {
	#[error(transparent)]
	DesignError(#[from] DesignError),

	#[error(transparent)]
	CodegenError(#[from] CodegenError),
}
