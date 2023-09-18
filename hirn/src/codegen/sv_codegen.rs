use std::fmt;
use crate::{Design, ModuleId, DesignError};
use super::{Codegen, CodegenError};

#[derive(Clone)]
pub struct SVCodegen<'a> {
	design: &'a Design,
}

impl<'a> SVCodegen<'a> {
	pub fn new(design: &'a Design) -> Self {
		Self {
			design
		}
	}
}

impl<'a> Codegen for SVCodegen<'a> {
	fn emit_module(&mut self, stream: &mut fmt::Formatter<'_>, module: ModuleId) -> Result<(), CodegenError> {
		let m = self.design.get_module_handle(module);
		writeln!(stream, "module {}({});", "name", "/* ... */");
		Ok(())
	}
}

#[cfg(test)]
mod test {
	use crate::*;
	use super::*;

	#[test]
	fn basic_codegen_test() -> Result<(), DesignError> {

		let mut d = Design::new();
		let mut m = d.new_module("test")?;

		let cg = SVCodegen::new(&d);


		Ok(())
	}
}