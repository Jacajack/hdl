use std::sync::Arc;

use assumptions::{ElabAssumptions, ElabAssumptionsBase};

use crate::{
	design::{BlockInstance, HasSensitivity, InterfaceSignal, ModuleInstance, Register, ScopeHandle, SignalId},
	elab::{assumptions, ElabMessageKind},
};

use super::MainPassCtx;

impl MainPassCtx {
	fn elab_reg_port(
		&mut self,
		sig_id: SignalId,
		drive: bool,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		if drive {
			self.drive_signal(&sig_id.into(), assumptions)?;
		}
		else {
			self.read_signal(&sig_id.into(), assumptions)?;
		}

		Ok(())
	}

	fn elab_instance_port(
		&mut self,
		scope: ScopeHandle,
		interface_sig: &InterfaceSignal,
		ext_sig: SignalId,
		instance_id: usize,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		if interface_sig.is_input() {
			self.assign_signals(
				scope,
				assumptions.clone(),
				&interface_sig.signal().into(),
				Some(instance_id),
				&ext_sig.into(),
				None,
			)
		}
		else {
			self.assign_signals(
				scope,
				assumptions.clone(),
				&ext_sig.into(),
				None,
				&interface_sig.signal().into(),
				Some(instance_id),
			)
		}
	}

	fn elab_register(
		&mut self,
		reg: &Register,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		self.elab_reg_port(reg.output_data, true, assumptions.clone())?;
		self.elab_reg_port(reg.input_clk, false, assumptions.clone())?;
		self.elab_reg_port(reg.input_en, false, assumptions.clone())?;
		self.elab_reg_port(reg.input_next, false, assumptions.clone())?;
		self.elab_reg_port(reg.input_nreset, false, assumptions.clone())?;
		Ok(())

		// TODO check bindings
		// TODO assert clock valid
		// TODO assert output width equals input width
		// TODO assert all other important things which Wojtek probably already did so it's never gonna get covered
	}

	fn elab_module_instance(
		&mut self,
		scope: ScopeHandle,
		instance: &ModuleInstance,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		let module_handle = instance.module.clone();
		let mut inner_assumptions = ElabAssumptions::new(Some(self.design.clone()));
		let mut interface_assumptions = ElabAssumptions::new_with_parent(assumptions.clone());

		// First, we resolve all generic assignments and make new assumptions
		for (port_name, sig_id) in instance.get_bindings() {
			let interface_sig = module_handle
				.get_interface_signal_by_name(port_name)
				.expect("Invalid port name");
			let inner_sig_id = interface_sig.signal();
			let inner_sig = module_handle.design().get_signal(inner_sig_id).unwrap();

			if !inner_sig.is_generic() {
				continue;
			}
			assert!(interface_sig.is_input());

			let assumption_value = assumptions
				.get(*sig_id)
				.expect("Cannot bind unassumed signal to instance")
				.clone();

			inner_assumptions.assume(inner_sig_id, assumption_value.clone());
			interface_assumptions.assume(inner_sig_id, assumption_value);
		}

		// Pack interface assumptions into Arc
		let interface_assumptions = Arc::new(interface_assumptions);

		for (port_name, sig_id) in instance.get_bindings() {
			let interface_sig = module_handle
				.get_interface_signal_by_name(port_name)
				.expect("Invalid port name");
			let inner_sig_id = interface_sig.signal();
			let inner_sig = module_handle.design().get_signal(inner_sig_id).unwrap();

			if inner_sig.is_generic() {
				continue;
			}

			self.declare_ext_interface_signal(
				module_handle.clone(),
				inner_sig_id,
				self.instance_counter,
				interface_assumptions.clone(),
			)?;

			self.elab_instance_port(
				scope.clone(),
				&interface_sig,
				*sig_id,
				self.instance_counter,
				interface_assumptions.clone(),
			)?;
		}

		// Inner assumptions are sufficient for module elaboration
		if module_handle.should_elaborate() {
			self.queued_modules.push((module_handle, Arc::new(inner_assumptions)));
		}

		Ok(())

		// TODO check bindings
	}

	pub(super) fn elab_block(
		&mut self,
		scope: ScopeHandle,
		block: &BlockInstance,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		self.instance_counter += 1;
		match block {
			BlockInstance::Register(reg) => self.elab_register(reg, assumptions),
			BlockInstance::Module(module) => self.elab_module_instance(scope, module, assumptions),
		}
	}
}
