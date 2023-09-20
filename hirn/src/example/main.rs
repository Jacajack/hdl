extern crate hirn;
use hirn::{SVCodegen, Codegen, HirnError, Design, design::SignalDirection};

fn main() -> Result<(), HirnError> {
	let mut d = Design::new();
	let mut m = d.new_module("test").unwrap();
	let m_clk = m.scope().new_signal()?.name("clk").clock().wire().build()?;
	let m_clkout = m.scope().new_signal()?.name("clkout").clock().wire().build()?;
	m.expose(m_clk, SignalDirection::Input)?;
	m.expose(m_clkout, SignalDirection::Output)?;

	let mut source = String::new();
	let cg = SVCodegen::new(&d);
	cg.emit_module(&mut source, m.id())?;

	println!("{}", source);

	Ok(())
}