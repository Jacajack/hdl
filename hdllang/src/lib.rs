#[macro_use] extern crate lalrpop_util;
extern crate derive_more;

pub mod diagnostic_buffer;
pub mod core;
pub mod lexer;
pub mod parser;

pub use crate::core::compiler_diagnostic;
pub use crate::core::CompilerDiagnostic;
pub use crate::core::CompilerError;
pub use crate::core::SourceSpan;
pub use diagnostic_buffer::DiagnosticBuffer;
