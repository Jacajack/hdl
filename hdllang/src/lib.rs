#[macro_use]
extern crate lalrpop_util;
extern crate derive_more;

pub mod analyzer;
pub mod core;
pub mod lexer;
pub mod parser;
mod hirn;

pub use crate::core::compiler_diagnostic;
pub use crate::core::CompilerDiagnostic;
pub use crate::core::CompilerError;
pub use crate::core::ProvidesCompilerDiagnostic;
pub use crate::core::SourceSpan;
