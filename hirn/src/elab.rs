mod assumptions;
mod elab_signal;
mod elaborator;
mod multi_pass_elab;
mod report;

pub use assumptions::{ElabAssumptions, ElabAssumptionsBase, ElabToplevelAssumptions};
pub use elab_signal::{ElabSignal, SignalMask};
pub use elaborator::Elaborator;
pub use multi_pass_elab::{FullElaborator, GeneratedSignal, GeneratedSignalId, GeneratedSignalRef, ScopePassId, ScopePassInfo};
pub use report::{
	DefaultSeverityPolicy, ElabMessage, ElabMessageKind, ElabMessageSeverity, ElabReport, SeverityPolicy,
};
use thiserror::Error;

use crate::design::EvalError;
pub type GenericVar = i64;

#[derive(Clone, Debug, Error)]
pub enum ElabError {
	#[error(transparent)]
	EvalError(#[from] EvalError),
	// #[error(transparent)]
	// ModuleElabFailed(#[from] ElabMessageKind),
}
