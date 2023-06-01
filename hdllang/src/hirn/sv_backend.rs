use super::*;
use super::indenter::Indenter;
use std::fmt;

trait HirnBackend {
	fn emit_module(&mut self, module: &Module) -> Result<(), fmt::Error>; // FIXME
}

struct SystemVerilogBackend<'stream> {
	output: &'stream mut dyn fmt::Write
}

impl<'stream> SystemVerilogBackend<'stream> {

	pub fn new(output: &'stream mut dyn fmt::Write) -> Self {
		Self {
			output
		}
	}

	fn format_signal_ref(&self, signal: &SignalRef) -> String {
		"signal_name[1][2][3:4]".into() // FIXME
	}

	fn mangle_module_name(&self, module: &Module) -> String {
		"MODULE_NAME".into() // FIXME
	}
	
	fn emit_register(&self, r: &Register) -> String {
		"// register".into() // FIXME
	}
}

impl<'a> HirnBackend for SystemVerilogBackend<'a> {
	
	fn emit_module(&mut self, module: &Module) -> Result<(), fmt::Error> {
		let module_name = self.mangle_module_name(module);
		let module_interface = "input wire test";
		let module_body = "// body";

		use std::fmt::Write;

		writeln!(self.output, "module {module_name}(");
		write!(super::indenter::indented!(self.output), "{module_interface}");
		writeln!(self.output, ");");
		write!(super::indenter::indented!(self.output), "{module_body}");
		writeln!(self.output, "endmodule");

		Ok(())
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn basic_test() {
		let mut string = String::new();
		let mut backend = SystemVerilogBackend::new(&mut string);
		let module = Module::new("test".into());
		backend.emit_module(&module);
		// assert_eq!(string, "module test(input wire test);\n\t// body\nendmodule");
		println!("{}", string);
	}
}