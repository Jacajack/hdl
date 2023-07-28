pub mod expression_ops;
pub mod indenter;
pub mod expression;
pub mod functional_blocks;
pub mod signal;
pub mod module;
pub mod backend;
pub mod sv_backend;

pub use module::{Module};
pub use expression::{Expression, BinaryOp, UnaryOp};
use signal::{SignalRef};

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn basic_test() {
		// let m = Module::new("test".into());

	}
}