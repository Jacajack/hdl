pub mod sv_backend;
pub mod indenter;

use super::Module;
use std::fmt;

trait HirnBackend {
	fn emit_module(&mut self, module: &Module) -> Result<(), fmt::Error>; // FIXME
}