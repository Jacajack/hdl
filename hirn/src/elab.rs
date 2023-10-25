mod elaborator;
mod assumptions;
mod report;
mod multi_pass_elab;

use thiserror::Error;
pub use report::{ElabMessage, ElabMessageKind, ElabMessageSeverity, ElabReport};
pub use elaborator::Elaborator;
pub use assumptions::{ElabAssumptions, ElabAssumptionsBase, ElabToplevelAssumptions};
pub use multi_pass_elab::FullElaborator;
pub type GenericVar = i64;

#[derive(Clone, Copy, Debug, Error)]
pub enum ElabError {
}