use crate::compiler_diagnostic::*;
use thiserror::Error;

#[derive(Copy, Clone, Error, Debug)]
pub enum SemanticError {
	#[error("Module has more than one `impl` block reffering to it")]
	MultipleModuleImplementations,
	#[error("It is not allowed to declare multiple packages at once")]
	MultiplePackageDeclaration,
	#[error("It is not allowed to package the same file multiple times")]
	FilePackagedMultipleTimes,
	#[error("It is not allowed to declare multiple qualifiers of the same type")]
	DuplicateQualifier,
	#[error("It is not allowed to declare a qualifier that contradicts another qualifier")]
	ContradictingQualifier,
	#[error("It is not allowed to declare a qualifier that contradicts specifier")]
	ContradictingSpecifier,
	#[error("It is not allowed to variable of type \"auto\" in a module declaration")]
	AutoSpecifierInDeclaration,
	#[error ("It is not allowed to declare a variable without a direction (\"input\" or \"output\") or without \"tristate\" qualifier")]
	MissingDirectionQualifier,
	#[error("It is not allowed to declare a variable with the same name as the other variable in this scope")]
	DuplicateVariableDeclaration,
	#[error("It is not allowed to declare multiple modules with the same name")]
	MultipleModuleDeclaration,
	#[error("Every signal in mentioned in with \"sync\" qualifier must be marked as clock")]
	NotClockSignalInSync,
	#[error ("It is not allowed to use other expression than identifier or negated identifier in \"sync\" and \"comb\" qualifiers")]
	ForbiddenExpressionInSyncOrComb,
	#[error("It is not allowed to use an identfier that does not represent any declared variable")]
	VariableNotDeclared,
	#[error("It is not allowed to use a non-generic type variable in an expression")]
	NonGenericTypeVariableInExpression,
	#[error("It is not allowed for variable to reference itself via qualifiers \"sync\" or \"comb\"")]
	VariableReferencingItself,
	#[error("It is not allowed to shift by a negative number")]
	ShiftByNegativeNumber,
	#[error("It is not allowed to divide by zero")]
	DivisionByZero,
	#[error("It is not allowed to use a negative number as a bus width")]
	NegativeBusWidth,
	#[error("Compiler could not find a generic module implementation for this module in the same file.")]
	GenericModuleImplementationNotFound,
	#[error("This expression is not allowed in a non-generic module declaration")]
	ExpressionNotAllowedInNonGenericModuleDeclaration,
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
			NotClockSignalInSync => CompilerDiagnosticBuilder::from_error(&self)
				.help("Mark proper signal as \"clock\"")
				.build(),
			ForbiddenExpressionInSyncOrComb => CompilerDiagnosticBuilder::from_error(&self)
				.help("Use only identifier or negated identifier in \"sync\" qualifier")
				.build(),
			VariableNotDeclared => CompilerDiagnosticBuilder::from_error(&self)
				.help("Please declare this variable, or change this identfier to the one that is declared.")
				.build(),
			NonGenericTypeVariableInExpression => CompilerDiagnosticBuilder::from_error(&self)
				.help("Use only generic type variables in expressions inside module declarations.")
				.build(),
			VariableReferencingItself => CompilerDiagnosticBuilder::from_error(&self)
				.help("Please remove this variable from its \"sync\" or \"comb\" qualifier.")
				.build(),
			ShiftByNegativeNumber => CompilerDiagnosticBuilder::from_error(&self)
				.help("Please use only positive numbers in shift expressions.")
				.build(),
			DivisionByZero => CompilerDiagnosticBuilder::from_error(&self)
				.help("Please use only non-zero numbers as a  divisors.")
				.build(),
			NegativeBusWidth => CompilerDiagnosticBuilder::from_error(&self)
				.help("Please use only positive numbers as a bus width.")
				.build(),
			GenericModuleImplementationNotFound => CompilerDiagnosticBuilder::from_error(&self)
				.help("Please provide implementation of this module in the same file.")
				.build(),
			ExpressionNotAllowedInNonGenericModuleDeclaration => CompilerDiagnosticBuilder::from_error(&self)
				.help("Please only allowed expressions in non-generic module declarations.")
				.build(),
		}
	}
}
