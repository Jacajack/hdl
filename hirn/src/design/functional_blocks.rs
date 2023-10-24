use std::collections::{HashSet, HashMap};

use log::debug;

use crate::design::HasSensitivity;

use super::{
	module::SignalDirection, DesignError, EvalContext, EvaluatesType, HasComment, ModuleHandle, ScopeHandle, SignalId,
};

pub trait HasInstanceName {
	fn instance_name(&self) -> &str;
}

/// Register block
#[derive(Clone, Debug)]
pub struct Register {
	/// Asynchronous negated reset input
	pub input_nreset: SignalId,

	/// Enable input
	pub input_en: SignalId,

	/// Clock input
	pub input_clk: SignalId,

	/// Next value input
	pub input_next: SignalId,

	/// Output value
	pub output_data: SignalId,

	/// Instance name
	pub name: String,

	/// Comment
	pub comment: Option<String>,
}

impl HasInstanceName for Register {
	fn instance_name(&self) -> &str {
		&self.name
	}
}

impl HasComment for Register {
	fn get_comment(&self) -> Option<String> {
		self.comment.clone()
	}
}

#[derive(Clone, Copy, Debug)]
pub enum ReqiuredRegisterSignal {
	Nreset,
	Clk,
	Next,
	Output,
	En,
}

/// A builder for register blocks
#[derive(Clone, Debug)]
pub struct RegisterBuilder {
	scope: ScopeHandle,
	input_nreset: Option<SignalId>,
	input_en: Option<SignalId>,
	input_clk: Option<SignalId>,
	input_next: Option<SignalId>,
	output_data: Option<SignalId>,
	name: String,
	comment: Option<String>,
}

impl RegisterBuilder {
	pub fn new(scope: ScopeHandle, name: &str) -> Self {
		Self {
			scope,
			input_nreset: None,
			input_en: None,
			input_clk: None,
			input_next: None,
			output_data: None,
			name: name.into(),
			comment: None,
		}
	}

	/// Connects a negated reset signal
	pub fn nreset(mut self, signal: SignalId) -> Self {
		assert!(self.input_nreset.is_none());
		self.input_nreset = Some(signal);
		self
	}

	/// Connects an enable signal
	pub fn en(mut self, signal: SignalId) -> Self {
		assert!(self.input_en.is_none());
		self.input_en = Some(signal);
		self
	}

	/// Connects a clock signal
	pub fn clk(mut self, signal: SignalId) -> Self {
		assert!(self.input_clk.is_none());
		self.input_clk = Some(signal);
		self
	}

	/// Connects a next value signal
	pub fn next(mut self, signal: SignalId) -> Self {
		assert!(self.input_next.is_none());
		self.input_next = Some(signal);
		self
	}

	/// Connects an output signal
	pub fn output(mut self, signal: SignalId) -> Self {
		assert!(self.output_data.is_none());
		self.output_data = Some(signal);
		self
	}

	pub fn comment(mut self, comment: &str) -> Self {
		self.name = comment.into();
		self
	}

	/// Creates the register
	pub fn build(mut self) -> Result<(), DesignError> {
		// TODO assert clock is a clock
		// TODO assert input is comb to this clock
		// TODO assert output is sync to this clock
		// TODO assert input and output have same width
		// TODO assert nreset is 1-bit
		// TODO assert en is 1-bit and comb to specified clock

		use ReqiuredRegisterSignal::*;
		self.scope.add_block(BlockInstance::Register(Register {
			input_clk: self
				.input_clk
				.ok_or(DesignError::RequiredRegisterSignalNotConnected(Clk))?,
			input_en: self
				.input_en
				.ok_or(DesignError::RequiredRegisterSignalNotConnected(En))?,
			input_nreset: self
				.input_nreset
				.ok_or(DesignError::RequiredRegisterSignalNotConnected(Nreset))?,
			input_next: self
				.input_next
				.ok_or(DesignError::RequiredRegisterSignalNotConnected(Next))?,
			output_data: self
				.output_data
				.ok_or(DesignError::RequiredRegisterSignalNotConnected(Output))?,
			name: self.name,
			comment: self.comment,
		}))
	}
}

/// Clock gating block
#[derive(Clone, Debug)]
pub struct ClockGate {
	/// Enable input
	pub input_en: SignalId,

	/// Clock input
	pub input_clk: SignalId,

	/// Output clock
	pub output_clk: SignalId,
}

impl HasInstanceName for ClockGate {
	fn instance_name(&self) -> &str {
		todo!();
	}
}

impl HasComment for ClockGate {
	fn get_comment(&self) -> Option<String> {
		todo!();
	}
}

// FF synchronizer block
#[derive(Clone, Debug)]
pub struct FfSync {
	/// Asynchronous negated reset input
	pub input_nreset: Option<SignalId>,

	/// Enable input
	pub input_en: Option<SignalId>,

	/// Input clock signal input
	pub input_clk1: SignalId,

	/// Output clock signal input
	pub input_clk2: SignalId,

	/// Next value input
	pub input_next: SignalId,

	/// Output value
	pub output_data: SignalId,
}

impl HasInstanceName for FfSync {
	fn instance_name(&self) -> &str {
		todo!();
	}
}

impl HasComment for FfSync {
	fn get_comment(&self) -> Option<String> {
		todo!();
	}
}

/// Represents an instantiated module
#[derive(Clone, Debug)]
pub struct ModuleInstance {
	/// ID of the instantiated module
	pub module: ModuleHandle,

	name: String,
	bindings: Vec<(String, SignalId)>,
	comment: Option<String>,
}

impl HasInstanceName for ModuleInstance {
	fn instance_name(&self) -> &str {
		&self.name
	}
}

impl HasComment for ModuleInstance {
	fn get_comment(&self) -> Option<String> {
		self.comment.clone()
	}
}

impl ModuleInstance {
	fn new(module: ModuleHandle, name: &str, bindings: Vec<(String, SignalId)>) -> Result<Self, DesignError> {
		let new = Self {
			module: module.clone(),
			name: name.into(),
			bindings,
			comment: None,
		};

		new.verify_bindings()?;
		Ok(new)
	}

	fn verify_bindings(&self) -> Result<(), DesignError> {
		let mut names = HashSet::new();
		debug!("Verifying instance '{}' (MID: {:?}) bindings...", self.instance_name(), self.module.id());

		// Map all binding names into internal signal IDs all at once
		let int_sig_ids_opt: Option<Vec<_>> = self.bindings.iter().map(
			|binding|{
				self.module.get_interface_signal_by_name(&binding.0)
				.map(|int_sig| int_sig.signal)
		}).collect();

		if int_sig_ids_opt.is_none() {
			return Err(DesignError::InvalidInterfaceSignalName(self.module.id()));
		}

		// This int_sig_ids has the same ordering as interface bindings
		let mut clock_map = HashMap::new();
		let int_sig_ids = int_sig_ids_opt.unwrap();
		assert_eq!(int_sig_ids.len(), self.bindings.len());

		// Find all clocks and build a clock map
		for (int_sig_id, (_int_name, ext_signal_id)) in int_sig_ids.iter().zip(self.bindings.iter()) {
			let design_handle = self.module.design();
			let design = design_handle.borrow();
			let int_sig = design.get_signal(*int_sig_id).expect("interface signal not in design");
			if int_sig.is_clock() {
				clock_map.insert(*int_sig_id, *ext_signal_id);
			}
		}

		debug!("Binding clock map: {clock_map:?}");

		// After the clock map is built, verify all bindings with the clock map
		for (name, signal) in &self.bindings {
			self.verify_binding(name, *signal, &clock_map)?;

			// Catch duplicate bindings
			if names.contains(name) {
				return Err(DesignError::DuplicateInterfaceBinding(self.module.id()));
			}

			names.insert(name);
		}

		Ok(())
	}

	fn verify_binding(&self, name: &str, extern_sig: SignalId, clock_map: &HashMap<SignalId, SignalId>) -> Result<(), DesignError> {
		debug!("Checking binding '{}' <-> {:?}", name, extern_sig);
		let intern_sig = self
			.module
			.get_interface_signal_by_name(name)
			.ok_or(DesignError::InvalidInterfaceSignalName(self.module.id()))?;

		// TODO defer this logic to assignment checker
		let eval_ctx = EvalContext::without_assumptions(self.module.design());
		let intern_type = intern_sig.signal.eval_type(&eval_ctx)?;
		let mut extern_type = extern_sig.eval_type(&eval_ctx)?;
		debug!("Interface type: {:?}", intern_type);
		debug!("Binding type: {:?}", extern_type);
		extern_type.sensitivity.substitute_clocks(clock_map);

		let lhs_type;
		let rhs_type;
		if intern_sig.direction == SignalDirection::Output {
			lhs_type = &extern_type;
			rhs_type = &intern_type;
		}
		else {
			lhs_type = &intern_type;
			rhs_type = &extern_type;
		}

		// Interface drives the expression
		if !rhs_type.can_drive(&lhs_type) {
			return Err(DesignError::IncompatibleBindingType {
				module: self.module.id(),
				signal: intern_sig.signal,
				interface_type: intern_type,
				binding_type: extern_type,
			});
		}

		Ok(())
	}

	pub fn get_bindings(&self) -> &Vec<(String, SignalId)> {
		&self.bindings
	}

	// FIXME Leaving this here for now cause it has some useful logic
	/*
	fn verify_binding(&self, name: &str, expr: &Expression) -> Result<(), DesignError> {
		let sig = self
			.module
			.get_interface_signal_by_name(name)
			.ok_or(DesignError::InvalidInterfaceSignalName(self.module.id()))?;

		// TODO defer this logic to assignment checker
		let eval_ctx = EvalContext::without_assumptions(self.module.design());
		let expr_type = expr.eval_type(&eval_ctx)?;
		let sig_type = sig.signal.eval_type(&eval_ctx)?;

		if sig.direction == SignalDirection::Output {
			// Interface drives the expression

			if expr.try_drive().is_none() {
				return Err(DesignError::ExpressionNotDriveable);
			}

			if !sig_type.can_drive(&expr_type) {
				return Err(DesignError::IncompatibleBindingType {
					module: self.module.id(),
					signal: sig.signal,
					interface_type: sig_type,
					binding_type: expr_type,
				});
			}
		}
		else {
			if !expr_type.can_drive(&sig_type) {
				return Err(DesignError::IncompatibleBindingType {
					module: self.module.id(),
					signal: sig.signal,
					interface_type: sig_type,
					binding_type: expr_type,
				});
			}
		}

		Ok(())
	}


	*/
}

#[derive(Debug)]
pub struct ModuleInstanceBuilder {
	module: ModuleHandle,
	scope: ScopeHandle,
	bindings: Vec<(String, SignalId)>,
	name: String,
}

impl ModuleInstanceBuilder {
	pub fn new(scope: ScopeHandle, module: ModuleHandle, name: &str) -> Self {
		Self {
			scope,
			module,
			bindings: vec![],
			name: name.into(),
		}
	}

	pub fn bind(mut self, name: &str, signal: SignalId) -> Self {
		self.bindings.push((name.into(), signal));
		self
	}

	pub fn build(mut self) -> Result<(), DesignError> {
		self.scope.add_block(BlockInstance::Module(ModuleInstance::new(
			self.module,
			&self.name,
			self.bindings,
		)?))
	}
}

/// Represents a functional block instance
#[derive(Clone, Debug)]
pub enum BlockInstance {
	Register(Register),
	Module(ModuleInstance),
}

impl HasComment for BlockInstance {
	fn get_comment(&self) -> Option<String> {
		use BlockInstance::*;
		match self {
			Register(r) => r.get_comment(),
			Module(m) => m.get_comment(),
		}
	}
}

impl HasInstanceName for BlockInstance {
	fn instance_name(&self) -> &str {
		use BlockInstance::*;
		match self {
			Register(r) => r.instance_name(),
			Module(m) => m.instance_name(),
		}
	}
}
