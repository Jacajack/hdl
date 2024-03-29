extern crate hirn;
use std::sync::Arc;

use hirn::{
	codegen::{sv::SVCodegen, Codegen},
	design::{DesignHandle, Expression, SignalDirection},
	elab::{ElabAssumptions, Elaborator},
	HirnError,
};

fn main() -> Result<(), HirnError> {
	let mut d = DesignHandle::new();

	let mut m_internal = d.new_module("inner_module").unwrap();
	let internal_clk = m_internal.scope().new_signal("clk")?.clock().wire().build()?;
	let internal_param = m_internal
		.scope()
		.new_signal("p")?
		.generic()
		.unsigned(64.into())
		.build()?;
	m_internal.expose(internal_clk, SignalDirection::Input)?;
	m_internal.expose(internal_param, SignalDirection::Input)?;

	let mut m = d.new_module("test").unwrap();
	let m_clk = m.scope().new_signal("clk")?.clock().wire().build()?;
	let m_nreset = m.scope().new_signal("nreset")?.asynchronous().wire().build()?;
	let m_clkout = m.scope().new_signal("clkout")?.clock().wire().build()?;
	let m_bus = m
		.scope()
		.new_signal("asdfg")?
		.asynchronous()
		.unsigned(8u32.into())
		.build()?;

	let m_output_bus = m
		.scope()
		.new_signal("reg_output")?
		.asynchronous()
		.unsigned(8u32.into())
		.build()?;

	let m_param = m.scope().new_signal("bingo")?.generic().unsigned(64.into()).build()?;
	m.expose(m_clk, SignalDirection::Input)?;
	m.expose(m_clkout, SignalDirection::Output)?;
	m.expose(m_param, SignalDirection::Input)?; // TODO do not allow output const signals or move to interface?
	m.scope()
		.assign(m_clk.into(), hirn::design::Expression::from(m_clkout) + m_clkout.into())?;

	m.scope()
		.new_register("my_register")?
		.clk(m_clk)
		.en(m_clk)
		.next(m_bus)
		.output(m_output_bus)
		.nreset(m_nreset)
		.build()?;

	m.scope().new_subscope()?;

	let (_if_block, mut else_block) = m.scope().if_else_scope(Expression::new_one())?;
	else_block.assign(m_clkout.into(), 1.into())?;

	m.scope()
		.new_module(m_internal.clone(), "cool_module")?
		.bind("clk", m_clk)
		.bind("p", m_param)
		.build()?;

	let (mut loop_scope, iter) = m.scope().loop_scope("index", 0.into(), 10.into())?;
	loop_scope.assign(m_bus.into(), iter.into())?;

	let mut elab = hirn::elab::FullElaborator::new(d.clone());
	let elab_result = elab.elaborate(m.id(), Arc::new(ElabAssumptions::new(Some(d.clone()))))?;
	println!("{:?}", elab_result.main_result().report());

	let mut source = String::new();
	let mut cg = SVCodegen::new(d.clone(), &mut source);
	cg.emit_module(m_internal.id())?;
	cg.emit_module(m.id())?;

	println!("{}", source);

	Ok(())
}
