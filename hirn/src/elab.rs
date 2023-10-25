mod elaborator;
mod assumptions;
mod report;
mod multi_pass_elab;

use thiserror::Error;
pub use report::{ElabMessage, ElabMessageKind, ElabMessageSeverity};
pub use elaborator::{Elaborator, ElabAssumptionsBase};
pub use assumptions::ElabAssumptions;
pub type GenericVar = i64;

#[derive(Clone, Copy, Debug, Error)]
pub enum ElabError {
}