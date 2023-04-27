use crate::compiler_diagnostic::*;
use thiserror::Error;

#[derive(Copy, Clone, Error, Debug)]
pub enum SemanticError {
	#[error("Module has more than one `impl` block reffering to it")]
	MultipleModuleImplementations,
}

impl ProvidesCompilerDiagnostic for SemanticError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		match self {
			Self::MultipleModuleImplementations => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each module must have exactly one `impl` block referring to it.")
				.build(),
		}
	}
}
