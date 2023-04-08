extern crate derive_more;

pub mod lexer;
pub mod source_span;
pub mod core;

pub use crate::core::CompilerDiagnostic;
pub use crate::core::CompilerError;
pub use source_span::SourceSpan;