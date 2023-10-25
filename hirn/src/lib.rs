pub mod codegen;
pub mod design;
pub mod elab;

use codegen::CodegenError;
use design::DesignError;
use elab::ElabError;
use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum HirnError {
	#[error(transparent)]
	DesignError(#[from] DesignError),

	#[error(transparent)]
	CodegenError(#[from] CodegenError),

	#[error(transparent)]
	ElabError(#[from] ElabError)
}
