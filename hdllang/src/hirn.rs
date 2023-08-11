pub mod backend;
pub mod design;
pub mod error;

pub use error::Error;
pub use design::{Module, Expression, BinaryOp, UnaryOp, SignalRef, ScopeRef, ModuleRef};
