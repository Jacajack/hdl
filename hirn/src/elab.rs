mod assumptions;
mod elab_signal;
mod elaborator;
mod multi_pass_elab;
mod report;

pub use assumptions::{ElabAssumptions, ElabAssumptionsBase};
pub use elab_signal::SignalMaskSummary;
pub use elab_signal::{ElabSignal, SignalMask};
pub use elaborator::Elaborator;
pub use multi_pass_elab::{
	FullElabResult, FullElaborator, GeneratedSignal, GeneratedSignalId, GeneratedSignalRef, PartialElabResult,
	ScopePassId, ScopePassInfo,
};
pub use report::{
	DefaultSeverityPolicy, ElabMessage, ElabMessageKind, ElabMessageSeverity, ElabReport, SeverityPolicy,
};
use thiserror::Error;

use crate::design::EvalError;
pub type GenericVar = i64;

#[derive(Clone, Debug, Error)]
pub enum ElabError {
	#[error(transparent)]
	EvalError(#[from] Box<EvalError>),
	// #[error(transparent)]
	// ModuleElabFailed(#[from] ElabMessageKind),
}

impl From<EvalError> for ElabError {
	fn from(e: EvalError) -> Self {
		Self::EvalError(Box::new(e))
	}
}
