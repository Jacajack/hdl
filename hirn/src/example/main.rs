extern crate hirn;
use hirn::{SVCodegen, Codegen, HirnError, Expression, Design, design::{SignalDirection, InterfaceSignal}};

fn main() -> Result<(), HirnError> {
	let mut d = Design::new();

	let mut m_internal = d.new_module("inner_module").unwrap();
	let internal_clk =m_internal.scope().new_signal("clk")?.clock().wire().build()?;
	m_internal.expose(internal_clk, SignalDirection::Input)?;

	let mut m = d.new_module("test").unwrap();
	let m_clk = m.scope().new_signal("clk")?.clock().wire().build()?;
	let m_clkout = m.scope().new_signal("clkout")?.clock().wire().build()?;
	let m_bus = m.scope().new_signal("asdfg")?.asynchronous().unsigned(8.into()).build()?;
	let m_param = m.scope().new_signal("bingo")?.constant().unsigned(8.into()).build()?;
	m.expose(m_clk, SignalDirection::Input)?;
	m.expose(m_clkout, SignalDirection::Output)?;
	m.expose(m_param, SignalDirection::Input)?; // TODO do not allow output const signals or move to interface?
	m.scope().assign(m_clk.into(),  hirn::design::Expression::from(m_clkout) + m_clkout.into())?;


	m.scope().new_subscope()?;

	m.scope().if_scope(hirn::Expression::new_one())?;

	m.scope().new_module(m_internal, "cool_module")?
		.bind("clk", m_clk.into())
		.build()?;

	let (mut loop_scope, iter) = m.scope().loop_scope("index", 0.into(), 10.into())?;
	loop_scope.assign(m_bus.into(), iter.into())?;

	let mut source = String::new();
	let mut cg = SVCodegen::new(&d);
	cg.emit_module(&mut source, m.id())?;

	println!("{}", source);

	Ok(())
}