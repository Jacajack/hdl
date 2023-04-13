#[macro_use] extern crate lalrpop_util;
extern crate derive_more;

pub mod core;
pub mod lexer;
pub mod parser;
pub mod source_span;

pub use crate::core::CompilerDiagnostic;
pub use crate::core::CompilerError;
pub use source_span::SourceSpan;