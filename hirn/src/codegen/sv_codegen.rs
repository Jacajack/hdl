use std::fmt;
use crate::{Design, ModuleId, DesignError, ModuleHandle, design::SignalDirection, design::SignalSignedness, design::InterfaceSignal};
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

	fn mangle_module_name(&self, name: String, namespaces: Vec<String>) -> String {
		let mut segments = namespaces;
		segments.push(name);
		segments.join("_")
	}

	fn module_interface_definition(&self, m: ModuleHandle, s: InterfaceSignal) -> String {
		let sig = self.design.get_signal(s.signal).unwrap();

		use SignalDirection::*;
		let direction_str = match s.direction {
			Input => "input",
			Output => "output",
			_ => unimplemented!(),
		};
		
		use SignalSignedness::*;
		let sign_str = match (sig.is_scalar(), sig.class.signedness) {
			(true, _) => "",
			(false, Signed) => "signed",
			(false, Unsigned) => "unsigned",
		};

		let bus_width_str = ""; // FIXME
		let array_size_str = ""; // FIXME

		format!("{} {} wire{} {}{}", direction_str, sign_str, bus_width_str, sig.name(), array_size_str)
	}
}

impl<'a> Codegen for SVCodegen<'a> {

	fn emit_module(&self, w: &mut dyn fmt::Write, module: ModuleId) -> Result<(), CodegenError> {
		let m = self.design.get_module_handle(module).ok_or(CodegenError::InvalidModuleId(module))?;
		writeln!(w, "module {}(", self.mangle_module_name(m.name(), m.namespace_path()))?;
		writeln!(w, "\t/* interface */")?;

		for sig in m.interface() {
			writeln!(w, "\t{},", self.module_interface_definition(m.clone(), sig))?; // FIXME trailing comma
		}

		writeln!(w, ");")?;
		writeln!(w, "endmodule;")?;
		Ok(())
	}
}

#[cfg(test)]
mod test {
	use crate::*;
	use super::*;

	#[test]
	fn basic_codegen_test() -> Result<(), HirnError> {

		let mut d = Design::new();
		let mut m = d.new_module("test").unwrap();
		let m_clk = m.scope().new_signal()?.name("clk").clock().wire().build()?;
		let m_clkout = m.scope().new_signal()?.name("clkout").clock().wire().build()?;
		m.expose(m_clk, SignalDirection::Input)?;
		m.expose(m_clkout, SignalDirection::Output)?;

		let mut source = String::new();
		let cg = SVCodegen::new(&d);
		cg.emit_module(&mut source, m.id())?;


		Ok(())
	}
}