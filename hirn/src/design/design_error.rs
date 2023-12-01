use thiserror::Error;

use super::{functional_blocks, EvalError, EvalType, ModuleId, ScopeId, SignalId, SignalSensitivity, SignalSignedness};

#[derive(Clone, Debug)]
pub struct IncomaptibleBindingTypeError {
	pub module: ModuleId,
	pub signal: SignalId,
	pub binding_type: EvalType,
	pub interface_type: EvalType,
}

#[derive(Clone, Debug)]
pub struct IncompatibleSignednessError {
	pub lhs_signedness: SignalSignedness,
	pub rhs_signedness: SignalSignedness,
}

#[derive(Clone, Debug)]
pub struct IncompatibleSensitivityError {
	pub lhs_sensitivity: SignalSensitivity,
	pub rhs_sensitivity: SignalSensitivity,
}

#[derive(Clone, Debug)]
pub struct SignalNameConflictError {
	pub scope: ScopeId,
	pub first: SignalId,
	pub second: SignalId,
}

#[derive(Clone, Debug)]
pub struct ModuleNameConflictError {
	pub first: ModuleId,
	pub second: ModuleId,
}

impl From<IncomaptibleBindingTypeError> for DesignError {
	fn from(err: IncomaptibleBindingTypeError) -> Self {
		Self::IncompatibleBindingType(Box::new(err))
	}
}

impl From<IncompatibleSignednessError> for DesignError {
	fn from(err: IncompatibleSignednessError) -> Self {
		Self::IncompatibleSignedness(Box::new(err))
	}
}

impl From<IncompatibleSensitivityError> for DesignError {
	fn from(err: IncompatibleSensitivityError) -> Self {
		Self::IncompatibleSensitivity(Box::new(err))
	}
}

impl From<SignalNameConflictError> for DesignError {
	fn from(err: SignalNameConflictError) -> Self {
		Self::SignalNameConflict(Box::new(err))
	}
}

impl From<ModuleNameConflictError> for DesignError {
	fn from(err: ModuleNameConflictError) -> Self {
		Self::ModuleNameConflict(Box::new(err))
	}
}

/// Represents an error that can occur during design construction.
/// Elaboration errors are not accounted for here.
#[derive(Clone, Debug, Error)]
pub enum DesignError {
	#[error("Invalid name")]
	InvalidName,

	#[error("Invalid module ID")]
	InvalidModuleId(ModuleId),

	#[error("Duplicate module interface binding")]
	DuplicateInterfaceBinding(ModuleId),

	#[error("Invalid interface signal name")]
	InvalidInterfaceSignalName(ModuleId),

	#[error("Signal width not specified")]
	SignalWidthNotSpecified,

	#[error("Signal class not specified")]
	SignalClassNotSpecified,

	#[error(transparent)]
	EvalError(#[from] EvalError),

	#[error("Expression cannot be driven - cannot bind to an output or use in assignment LHS")]
	ExpressionNotDrivable, // TODO more details

	#[error("Signal sensitivity not specified")]
	SignalSensitivityNotSpecified,

	#[error("Conflicting signal sensitivity")]
	ConflictingSignalSensitivity,

	#[error("Required register signal is not connected")]
	RequiredRegisterSignalNotConnected(functional_blocks::ReqiuredRegisterSignal),

	#[error("Signal name conflict in scope")]
	SignalNameConflict(Box<SignalNameConflictError>),

	#[error("Module name conflict")]
	ModuleNameConflict(Box<ModuleNameConflictError>),

	#[error("Incompatible types in binding")]
	IncompatibleBindingType(Box<IncomaptibleBindingTypeError>),

	#[error("Incompatible signedness in assignment")]
	IncompatibleSignedness(Box<IncompatibleSignednessError>),

	#[error("Incompatible sensitivity in assignment")]
	IncompatibleSensitivity(Box<IncompatibleSensitivityError>),

	#[error("Signal width must be a constant expression")]
	VariableSignalWidth,

	#[error("Invalid signal width (must be positive)")]
	InvalidSignalWidth,

	#[error("Array dimensions must be constant expressions")]
	VariableArrayDimension,

	#[error("Invalid array dimension (must be positive)")]
	InvalidArrayDimension,

	#[error("Loop range bounds must be signed and generic")]
	InvalidLoopRange,

	#[error("If condition must be generic boolean (1-bit unsigned)")]
	InvalidIfCondition,
}
