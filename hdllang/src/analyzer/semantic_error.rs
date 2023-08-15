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
	#[error ("It is not allowed to declare a qualifier that contradicts specifier")]
	ContradictingSpecifier,
	#[error ("It is not allowed to variable of type \"auto\" in a module declaration")]
	AutoSpecifierInDeclaration,
	#[error ("It is not allowed to declare a variable without a direction (\"input\" or \"output\") or without \"tristate\" qualifier")]
	MissingDirectionQualifier,
	#[error ("It is not allowed to declare a variable with the same name as the other variable in this scope")]
	DuplicateVariableDeclaration,
	#[error ("It is not allowed to declare multiple modules with the same name")]
	MultipleModuleDeclaration
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
    		ContradictingSpecifier => CompilerDiagnosticBuilder::from_error(&self)
				.help("Qualifier must not contradict specifier.")
				.build(),
    		AutoSpecifierInDeclaration => CompilerDiagnosticBuilder::from_error(&self)
				.help("Specify the type of this variable explicitly.")
				.build(),
    		DuplicateVariableDeclaration => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each variable's name must be unique.")
				.build(),
    		MultipleModuleDeclaration => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each module must be declared only once.")
				.build(),
    		MissingDirectionQualifier => CompilerDiagnosticBuilder::from_error(&self)
				.help("Each variable must have a direction qualifier or be marked as \"tristate\"")
				.build(),
		}
	}
}
