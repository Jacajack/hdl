pub mod backend;
pub mod design;
pub mod error;

pub use error::Error;
pub use design::{Module};
pub use design::{Expression, BinaryOp, UnaryOp};
use design::{SignalRef};
pub use design::{Scope};
