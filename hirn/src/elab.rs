mod assumptions;
mod elaborator;
mod multi_pass_elab;
mod report;
mod elab_signal;

pub use assumptions::{ElabAssumptions, ElabAssumptionsBase, ElabToplevelAssumptions};
pub use elaborator::Elaborator;
pub use multi_pass_elab::FullElaborator;
pub use report::{ElabMessage, ElabMessageKind, ElabMessageSeverity, ElabReport, SeverityPolicy, DefaultSeverityPolicy};
pub use elab_signal::{SignalMask, ElabSignal};
use thiserror::Error;

use crate::design::EvalError;
pub type GenericVar = i64;

#[derive(Clone, Debug, Error)]
pub enum ElabError {
	#[error(transparent)]
	EvalError(#[from] EvalError),

	#[error("Too many for iterations")]
	MaxForIterCount, // FIXME iterator signal ID

	#[error("Cyclic generic variable dependency")]
	CyclicGenericDependency, // FIXME signal IDs

	#[error("Target signal is not drivable")]
	NotDrivable,
}
