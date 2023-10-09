pub mod codegen;
pub mod design;

pub use design::{Design, DesignError, BinaryOp, Expression, Module, ModuleId, ModuleHandle, ScopeHandle, ScopeId, SignalId, UnaryOp};
pub use codegen::{sv_codegen::SVCodegen, Codegen, CodegenError};

use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum HirnError {
	#[error(transparent)]
	DesignError(#[from] DesignError),

	#[error(transparent)]
	CodegenError(#[from] CodegenError),
}
