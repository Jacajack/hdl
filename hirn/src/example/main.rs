extern crate hirn;
use hirn::{SVCodegen, Codegen, HirnError, Design, design::{SignalDirection, InterfaceSignal}};

fn main() -> Result<(), HirnError> {
	let mut d = Design::new();

	let mut m_internal = d.new_module("inner_module").unwrap();
	let internal_clk =m_internal.scope().new_signal()?.name("clk").clock().wire().build()?;
	m_internal.expose(internal_clk, SignalDirection::Input)?;

	let mut m = d.new_module("test").unwrap();
	let m_clk = m.scope().new_signal()?.name("clk").clock().wire().build()?;
	let m_clkout = m.scope().new_signal()?.name("clkout").clock().wire().build()?;
	let m_bus = m.scope().new_signal()?.name("basdasd").asynchronous().unsigned(8.into()).build()?;
	m.expose(m_clk, SignalDirection::Input)?;
	m.expose(m_clkout, SignalDirection::Output)?;
	m.scope().assign(m_clk.into(),  hirn::design::Expression::from(m_clkout) + m_clkout.into())?;


	m.scope().new_subscope()?;

	m.scope().if_scope(hirn::Expression::new_one())?;

	m.scope().new_module(m_internal)?
		.bind("clk", m_clk.into())
		.build()?;

	let mut source = String::new();
	let mut cg = SVCodegen::new(&d);
	cg.emit_module(&mut source, m.id())?;

	println!("{}", source);

	Ok(())
}