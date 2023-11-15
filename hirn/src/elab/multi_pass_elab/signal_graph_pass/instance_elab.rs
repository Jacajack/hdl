use std::sync::Arc;

use assumptions::{ElabAssumptions, ElabAssumptionsBase};
use log::warn;

use crate::{
	design::{BlockInstance, EvalAssumptions, HasSensitivity, ModuleInstance, Register, SignalId},
	elab::{assumptions, ElabMessageKind},
};

use super::SignalGraphPassCtx;

impl SignalGraphPassCtx {
	fn elab_instance_port(
		&mut self,
		sig_id: SignalId,
		drive: bool,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		// Ignore generics
		let sig = self.design.get_signal(sig_id).unwrap();
		if sig.is_generic() {
			warn!("Ignoring generic signal {:?} in elab_instance_port()", sig_id);
			return Ok(());
		}

		if drive {
			self.drive_signal(&sig_id.into(), assumptions)?;
		}
		else {
			self.read_signal(&sig_id.into(), assumptions)?;
		}

		Ok(())
	}

	fn elab_register(
		&mut self,
		reg: &Register,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		self.elab_instance_port(reg.output_data, true, assumptions.clone())?;
		self.elab_instance_port(reg.input_clk, false, assumptions.clone())?;
		self.elab_instance_port(reg.input_en, false, assumptions.clone())?;
		self.elab_instance_port(reg.input_next, false, assumptions.clone())?;
		self.elab_instance_port(reg.input_nreset, false, assumptions.clone())?;
		Ok(())

		// TODO check bindings
		// TODO assert clock valid
		// TODO assert output width equals input width
		// TODO assert all other important things which Wojtek probably already did so it's never gonna get covered
	}

	fn elab_module_instance(
		&mut self,
		instance: &ModuleInstance,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		let module_handle = instance.module.clone();
		let mut inner_assumptions = ElabAssumptions::new_with_parent(assumptions.clone());

		for (port_name, sig_id) in instance.get_bindings() {
			let interface_sig = module_handle
				.get_interface_signal_by_name(&port_name)
				.expect("Invalid port name");
			let inner_sig_id = interface_sig.signal();
			let inner_sig = module_handle.design().get_signal(inner_sig_id).unwrap();

			// Make new assumption or process signal binding
			if inner_sig.is_generic() {
				inner_assumptions.assume(
					inner_sig_id,
					assumptions
						.get(*sig_id)
						.expect("Cannot bind unassumed signal to instance")
						.clone(),
				);
			}
			else {
				self.elab_instance_port(*sig_id, !interface_sig.is_input(), assumptions.clone())?;
			}
		}

		// TODO Important: trigger elab for module

		Ok(())

		// TODO check bindings
	}

	pub(super) fn elab_block(
		&mut self,
		block: &BlockInstance,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		match block {
			BlockInstance::Register(reg) => self.elab_register(reg, assumptions),
			BlockInstance::Module(module) => self.elab_module_instance(module, assumptions),
		}
	}
}
