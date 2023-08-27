use std::collections::HashSet;

use super::{
	eval::EvaluatesType, module::SignalDirection, DesignCore, DesignError, DesignHandle, EvalContext, Expression,
	ModuleHandle, ModuleId, ScopeHandle, ScopeId,
};

/// Register block
pub struct Register {
	/// Asynchronous negated reset input
	pub input_nreset: Expression,

	/// Enable input
	pub input_en: Expression,

	/// Clock input
	pub input_clk: Expression,

	/// Next value input
	pub input_next: Expression,

	/// Output value
	pub output_data: Expression,
}

#[derive(Clone, Copy, Debug)]
pub enum ReqiuredRegisterSignal {
	Nreset,
	Clk,
	Next,
	Output,
}

/// A builder for register blocks
pub struct RegisterBuilder {
	scope: ScopeHandle,
	input_nreset: Option<Expression>,
	input_en: Option<Expression>,
	input_clk: Option<Expression>,
	input_next: Option<Expression>,
	output_data: Option<Expression>,
}

impl RegisterBuilder {
	pub fn new(scope: ScopeHandle) -> Self {
		Self {
			scope,
			input_nreset: None,
			input_en: None,
			input_clk: None,
			input_next: None,
			output_data: None,
		}
	}

	/// Connects a negated reset signal
	pub fn nreset(mut self, expr: Expression) -> Self {
		assert!(self.input_nreset.is_none());
		self.input_nreset = Some(expr);
		self
	}

	/// Connects an enable signal
	pub fn en(mut self, expr: Expression) -> Self {
		assert!(self.input_en.is_none());
		self.input_en = Some(expr);
		self
	}

	/// Connects a clock signal
	pub fn clk(mut self, signal: Expression) -> Self {
		assert!(self.input_clk.is_none());
		self.input_clk = Some(signal);
		self
	}

	/// Connects a next value signal
	pub fn next(mut self, expr: Expression) -> Self {
		assert!(self.input_next.is_none());
		self.input_next = Some(expr);
		self
	}

	/// Connects an output signal
	pub fn output(mut self, signal: Expression) -> Self {
		assert!(self.output_data.is_none());
		self.output_data = Some(signal);
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
			input_en: self.input_en.unwrap_or(Expression::new_one()),
			input_nreset: self
				.input_nreset
				.ok_or(DesignError::RequiredRegisterSignalNotConnected(Nreset))?,
			input_next: self
				.input_next
				.ok_or(DesignError::RequiredRegisterSignalNotConnected(Next))?,
			output_data: self
				.output_data
				.ok_or(DesignError::RequiredRegisterSignalNotConnected(Output))?,
		}))
	}
}

/// Clock gating block
pub struct ClockGate {
	/// Enable input
	pub input_en: Expression,

	/// Clock input
	pub input_clk: Expression,

	/// Output clock
	pub output_clk: Expression,
}

// FF synchronizer block
pub struct FfSync {
	/// Asynchronous negated reset input
	pub input_nreset: Option<Expression>,

	/// Enable input
	pub input_en: Option<Expression>,

	/// Input clock signal input
	pub input_clk1: Expression,

	/// Output clock signal input
	pub input_clk2: Expression,

	/// Next value input
	pub input_next: Expression,

	/// Output value
	pub output_data: Expression,
}

/// Represents an instantiated module
pub struct ModuleInstance {
	/// ID of the instantiated module
	pub module: ModuleHandle,

	bindings: Vec<(String, Expression)>,
}

impl ModuleInstance {
	fn new(module: ModuleHandle, bindings: Vec<(String, Expression)>) -> Result<Self, DesignError> {
		let new = Self {
			module: module.clone(),
			bindings,
		};

		new.verify_bindings()?;
		Ok(new)
	}

	fn verify_bindings(&self) -> Result<(), DesignError> {
		let mut names = HashSet::new();

		for (name, expr) in &self.bindings {
			self.verify_binding(name, expr)?;

			// Catch duplicate bindings
			if names.contains(name) {
				return Err(DesignError::DuplicateInterfaceBinding(self.module.id()));
			}

			names.insert(name);
		}

		Ok(())
	}

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
}

pub struct ModuleInstanceBuilder {
	module: ModuleHandle,
	scope: ScopeHandle,
	bindings: Vec<(String, Expression)>,
}

impl ModuleInstanceBuilder {
	pub fn new(scope: ScopeHandle, module: ModuleHandle) -> Self {
		Self {
			scope,
			module,
			bindings: vec![],
		}
	}

	pub fn bind(mut self, name: &str, expr: Expression) -> Self {
		self.bindings.push((name.into(), expr));
		self
	}

	pub fn build(mut self) -> Result<(), DesignError> {
		self.scope
			.add_block(BlockInstance::Module(ModuleInstance::new(self.module, self.bindings)?))
	}
}

/// Represents a functional block instance
pub enum BlockInstance {
	Register(Register),
	ClockGate(ClockGate),
	FfSync(FfSync),
	Module(ModuleInstance),
}
