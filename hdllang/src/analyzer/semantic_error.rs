use crate::compiler_diagnostic::*;
use thiserror::Error;

#[derive(Copy, Clone, Error, Debug)]
pub enum SemanticError {
	#[error("Module has more than one `impl` block reffering to it")]
	MultipleModuleImplementations,
	#[error ("It is not allowed to declare multiple packages at once")]
	MultiplePackageDeclaration,
	#[error ("It is not allowed to package the same file multiple times")]
	FilePackagedMultipleTimes,
	#[error ("It is not allowed to declare multiple qualifiers of the same type")]
	DuplicateQualifier,
	#[error ("It is not allowed to declare a qualifier that contradicts another qualifier")]
	ContradictingQualifier,
}

impl ProvidesCompilerDiagnostic for SemanticError {
	fn to_diagnostic(&self) -> CompilerDiagnostic {
		use SemanticError::*;
		match self {
			MultipleModuleImplementations => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each module must have exactly one `impl` block referring to it.")
				.build(),
			MultiplePackageDeclaration => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each package must be declared separately.")
				.build(),
    		FilePackagedMultipleTimes => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each file can be packaged only once.")
				.build(),
    		DuplicateQualifier => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each qualifier must be declared only once.")
				.build(),
    		ContradictingQualifier => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each qualifier must not contradict another qualifier.")
				.build(),
		}
	}
}
