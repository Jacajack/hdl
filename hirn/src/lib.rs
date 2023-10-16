pub mod codegen;
pub mod design;

pub use codegen::{sv_codegen::SVCodegen, Codegen, CodegenError};
pub use design::{
	BinaryOp, Design, DesignError, Expression, Module, ModuleHandle, ModuleId, ScopeHandle, ScopeId, SignalId, UnaryOp,
};

use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum HirnError {
	#[error(transparent)]
	DesignError(#[from] DesignError),

	#[error(transparent)]
	CodegenError(#[from] CodegenError),
}
