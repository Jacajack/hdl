mod comment;
mod design_error;
mod expression;
mod functional_blocks;
mod module;
mod scope;
mod signal;
mod utils;

pub use comment::HasComment;
pub use design_error::*;
pub use expression::{
	BinaryExpression, BinaryOp, BuiltinOp, CastExpression, ConditionalExpression, ConditionalExpressionBranch,
	EvalAssumptions, EvalContext, EvalError, EvalType, Evaluates, EvaluatesType, Expression, NumericConstant,
	UnaryExpression, UnaryOp, WidthExpression,
};
pub use functional_blocks::{
	BlockInstance, HasInstanceName, ModuleInstance, ModuleInstanceBuilder, Register, RegisterBuilder,
};
use log::debug;
pub use module::{InterfaceSignal, Module, ModuleHandle, SignalDirection};
pub use scope::{ConditionalScope, RangeScope, Scope, ScopeHandle};
pub use signal::{
	ClockSensitivityList, EdgeSensitivity, HasSensitivity, HasSignedness, Signal, SignalBuilder, SignalClass,
	SignalSensitivity, SignalSignedness, SignalSlice, SignalSliceRange,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

/// References a module in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd, Debug)]
pub struct ModuleId {
	id: usize,
}

impl ModuleId {
	/// Checks if the reference is valid
	pub fn is_null(&self) -> bool {
		self.id == 0
	}
}

/// References a signal in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd, Debug)]
pub struct SignalId {
	id: usize,
}

/// References a signal in a design
impl SignalId {
	/// Checks if the reference is valid
	pub fn is_null(&self) -> bool {
		self.id == 0
	}

	// TODO implement as trait
	pub fn index(&self, index: Expression) -> SignalSlice {
		SignalSlice {
			signal: *self,
			indices: vec![index],
		}
	}
}

/// References a scope in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ScopeId {
	id: usize,
}

impl ScopeId {
	/// Checks if the reference is valid
	pub fn is_null(&self) -> bool {
		self.id == 0
	}
}

/// Core part of the generic design representation
/// Refferred to via multiple handles with reference
/// counting.
pub struct DesignCore {
	weak: Weak<RefCell<DesignCore>>,
	modules: Vec<Module>,
	scopes: Vec<Scope>,
	signals: Vec<Signal>,
	next_module_id: usize,
	next_scope_id: usize,
	next_signal_id: usize,
	scope_signals: HashMap<ScopeId, Vec<SignalId>>,
	scope_scopes: HashMap<ScopeId, Vec<ScopeId>>,
	module_scopes: HashMap<ModuleId, Vec<ScopeId>>,
}

impl DesignCore {
	fn get_handle(&self) -> DesignHandle {
		self.weak.upgrade().unwrap().into()
	}

	/// Creates a new scope and adds it to the design
	fn new_scope(&mut self, module: ModuleId, parent_scope: Option<ScopeId>) -> Result<ScopeHandle, DesignError> {
		let id = ScopeId { id: self.next_scope_id };
		self.next_scope_id += 1;

		let scope = Scope::new(id, module, parent_scope);
		self.scopes.push(scope);
		self.module_scopes.entry(module).or_insert(vec![]).push(id);

		if parent_scope.is_some() {
			self.scope_scopes
				.entry(parent_scope.unwrap())
				.or_insert(vec![])
				.push(id);
		}

		Ok(ScopeHandle::new(self.get_handle(), id))
	}

	/// Adds an existing signal to the design
	///
	/// Performs check for conflicting signal names.
	fn add_signal(&mut self, signal: Signal) -> Result<SignalId, DesignError> {
		let id = self.next_signal_id;
		self.next_signal_id += 1;

		let mut sig = signal;
		sig.id = SignalId { id };

		// Check for name collisions
		for id in self.get_scope_signals(sig.parent_scope).unwrap_or(&vec![]) {
			let other = self.get_signal(*id).unwrap();
			if other.name == sig.name {
				Err(SignalNameConflictError {
					scope: sig.parent_scope,
					first: other.id,
					second: sig.id,
				})?;
			}
		}

		// Add to design
		self.scope_signals
			.entry(sig.parent_scope)
			.or_insert(vec![])
			.push(SignalId { id });
		self.signals.push(sig);
		debug!(
			"Added signal '{}' ({:?}) to design",
			self.signals.last().unwrap().name(),
			self.signals.last().unwrap().id
		);
		Ok(SignalId { id })
	}

	/// Adds an existing module to the design
	fn add_module(&mut self, module: Module) -> Result<ModuleHandle, DesignError> {
		let id = self.next_module_id;
		self.next_module_id += 1;

		let mut m = module;
		m.id = ModuleId { id };

		// Check name conflicts
		// FIXME this ignores namespaces for now
		for other in &self.modules {
			if other.name == m.name {
				Err(ModuleNameConflictError {
					first: other.id,
					second: m.id,
				})?;
			}
		}

		self.modules.push(m);
		Ok(ModuleHandle::new(self.get_handle(), ModuleId { id }))
	}

	/// Returns a mutable reference to the signal with the given ID
	fn get_scope_mut(&mut self, scope: ScopeId) -> Option<&mut Scope> {
		self.scopes.get_mut(scope.id - 1)
	}

	/// Returns a reference to the scope with the given ID
	fn _get_scope(&self, scope: ScopeId) -> Option<&Scope> {
		self.scopes.get(scope.id - 1)
	}

	/// Returns a handle to the scope with the given ID
	fn get_scope_handle(&self, scope: ScopeId) -> Option<ScopeHandle> {
		Some(ScopeHandle::new(self.get_handle(), scope))
	}

	/// Returns a mutable reference to the module with the given ID
	fn get_module_mut(&mut self, module: ModuleId) -> Option<&mut Module> {
		self.modules.get_mut(module.id - 1)
	}

	/// Returns a reference to the module with the given ID
	fn get_module(&self, module: ModuleId) -> Option<&Module> {
		self.modules.get(module.id - 1)
	}

	/// Returns a handle to the module with the given ID
	fn get_module_handle(&self, module: ModuleId) -> Option<ModuleHandle> {
		Some(ModuleHandle::new(self.get_handle(), module))
	}

	/// Returns a reference to the signal with the given ID
	fn get_signal(&self, signal: SignalId) -> Option<&Signal> {
		self.signals.get(signal.id - 1)
	}

	/// Returns a mutable reference to the signal with the given ID
	fn get_signal_mut(&mut self, signal: SignalId) -> Option<&mut Signal> {
		self.signals.get_mut(signal.id - 1)
	}

	/// Returns a list of signals in the given scope
	fn get_scope_signals(&self, scope: ScopeId) -> Option<&Vec<SignalId>> {
		self.scope_signals.get(&scope)
	}

	fn get_scope_scopes(&self, scope: ScopeId) -> Option<&Vec<ScopeId>> {
		self.scope_scopes.get(&scope)
	}

	/// Creates a new module in the design
	fn new_module(&mut self, name: &str) -> Result<ModuleHandle, DesignError> {
		let module = Module::new(name, vec![])?;
		let handle = self.add_module(module)?;
		let main_scope = self.new_scope(handle.id(), None)?;
		self.get_module_mut(handle.id()).unwrap().main_scope = main_scope.id();
		Ok(handle)
	}
}

impl Default for DesignCore {
	fn default() -> Self {
		Self {
			weak: Weak::<RefCell<DesignCore>>::new(),
			modules: Vec::new(),
			scopes: Vec::new(),
			signals: Vec::new(),
			next_module_id: 1,
			next_scope_id: 1,
			next_signal_id: 1,
			scope_signals: HashMap::new(),
			scope_scopes: HashMap::new(),
			module_scopes: HashMap::new(),
		}
	}
}

/// Represents a hardware design
#[derive(Clone)]
pub struct DesignHandle {
	handle: Rc<RefCell<DesignCore>>,
}

impl std::fmt::Debug for DesignHandle {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "(DesignHandle")?;
		Ok(())
	}
}

impl DesignHandle {
	/// Creates a new HIRN design
	pub fn new() -> Self {
		let d = Self {
			handle: Rc::new(RefCell::new(DesignCore::default())),
		};

		d.handle.borrow_mut().weak = Rc::downgrade(&d.handle);
		d
	}

	fn borrow_mut(&self) -> std::cell::RefMut<DesignCore> {
		self.handle.borrow_mut()
	}

	fn borrow(&self) -> std::cell::Ref<DesignCore> {
		self.handle.borrow()
	}

	/// Creates a new module with provided name and returns a handle to it
	pub fn new_module(&mut self, name: &str) -> Result<ModuleHandle, DesignError> {
		self.handle.borrow_mut().new_module(name)
	}

	pub fn get_module_handle(&self, module: ModuleId) -> Option<ModuleHandle> {
		self.handle.borrow().get_module_handle(module)
	}

	pub fn get_scope_handle(&self, scope: ScopeId) -> Option<ScopeHandle> {
		self.handle.borrow().get_scope_handle(scope)
	}

	pub fn get_signal(&self, signal: SignalId) -> Option<Signal> {
		self.handle.borrow().get_signal(signal).cloned()
	}
}

impl From<Rc<RefCell<DesignCore>>> for DesignHandle {
	fn from(handle: Rc<RefCell<DesignCore>>) -> Self {
		Self { handle }
	}
}

#[cfg(test)]
mod test {
	use super::{expression::Evaluates, *};

	#[test]
	pub fn design_basic_test() -> Result<(), DesignError> {
		// init();
		let mut d = DesignHandle::new();
		let m = d.new_module("test")?;

		let sig = m
			.scope()
			.new_signal("test_signal")?
			.unsigned(37.into())
			.constant()
			.build()?;

		let expr = Expression::from(sig) + sig.into();

		let sig2 = m
			.scope()
			.new_signal("test_signal_2")?
			.unsigned(Expression::from(1u32) + 11u32.into())
			.constant()
			.build()?;

		let mut scope2 = m.scope().if_scope(Expression::new_zero())?;

		let sig_fancy_reg_en = scope2
			.new_signal("fancy_reg_en")?
			.unsigned(Expression::new_one())
			.constant()
			.build()?;

		let sig_fancy_reg_next = scope2
			.new_signal("fancy_reg_next")?
			.unsigned(11.into())
			.comb(sig, true)
			.build()?;

		scope2.assign(
			sig_fancy_reg_next.into(),
			Expression::new_conditional(false.into())
				.branch(1u32.into(), true.into())
				.build(),
		)?;

		let sig_fancy_reg_nreset = scope2
			.new_signal("fancy_reg_nreset")?
			.unsigned(Expression::new_one())
			.asynchronous()
			.build()?;

		scope2
			.new_register("fancy_reg")?
			.clk(sig)
			.nreset(sig_fancy_reg_nreset)
			.next(sig_fancy_reg_next)
			.en(sig_fancy_reg_en)
			.output(sig2)
			.build()?;

		scope2.assign(sig2.into(), expr)?;

		Ok(())
	}

	/// Verifies if the design correctly detects signal name conflicts
	#[test]
	fn test_unique_signal_names() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let m = d.new_module("test")?;

		let _sig = m
			.scope()
			.new_signal("name")?
			.unsigned(Expression::new_one())
			.constant()
			.build()?;

		let sig2 = m
			.scope()
			.new_signal("name")?
			.unsigned(Expression::new_one())
			.constant()
			.build();

		assert!(matches!(sig2, Err(DesignError::SignalNameConflict { .. })));
		Ok(())
	}

	/// Verifies if the design enforces unique module names
	#[test]
	fn test_unique_module_names() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let _m = d.new_module("name")?;
		let m2 = d.new_module("name");

		assert!(matches!(m2, Err(DesignError::ModuleNameConflict { .. })));
		Ok(())
	}

	/// Verify module naming rules
	#[test]
	fn test_module_naming_rules() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		assert!(matches!(d.new_module("asdf"), Ok(..)));
		assert!(matches!(d.new_module("_asdf1131"), Ok(..)));
		assert!(matches!(d.new_module("_asd_____f1131fafa_222"), Ok(..)));

		assert!(matches!(d.new_module("$wongo_bongo"), Err(DesignError::InvalidName)));
		assert!(matches!(
			d.new_module("1love5ystemVerilog"),
			Err(DesignError::InvalidName)
		));
		assert!(matches!(
			d.new_module("spaces are not allowed"),
			Err(DesignError::InvalidName)
		));
		Ok(())
	}

	/// Test register creation
	#[test]
	fn test_register() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let m = d.new_module("foo")?;

		let clk = m
			.scope()
			.new_signal("clk")?
			.clock()
			.unsigned(Expression::new_one())
			.build()?;

		let nreset = m
			.scope()
			.new_signal("nreset")?
			.asynchronous()
			.unsigned(Expression::new_one())
			.build()?;

		let next = m
			.scope()
			.new_signal("next")?
			.unsigned(Expression::new_one())
			.comb(clk, true)
			.build()?;

		let en = m
			.scope()
			.new_signal("en")?
			.unsigned(Expression::new_one())
			.comb(clk, true)
			.build()?;

		let output = m
			.scope()
			.new_signal("output")?
			.unsigned(Expression::new_one())
			.sync(clk, true)
			.build()?;

		m.scope()
			.new_register("useless_reg")?
			.clk(clk)
			.en(en)
			.nreset(nreset)
			.next(next)
			.output(output)
			.build()?;

		Ok(())
	}

	/// Test module interface binding
	#[test]
	fn test_interfaces() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let mut m = d.new_module("foo")?;
		let m_clk = m.scope().new_signal("clk")?.clock().wire().build()?;

		m.expose(m_clk, SignalDirection::Input)?;

		let m_parent = d.new_module("bar")?;
		let m_parent_clk = m_parent.scope().new_signal("clk")?.clock().wire().build()?;

		m_parent
			.scope()
			.new_module(m, "child_module")?
			.bind("clk", m_parent_clk)
			.build()?;

		Ok(())
	}

	/// Test invalid module interface binding async->clk
	#[test]
	fn test_bad_interface_binding() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let mut m = d.new_module("foo")?;
		let m_clk = m.scope().new_signal("clk")?.clock().wire().build()?;

		m.expose(m_clk, SignalDirection::Input)?;

		let m_parent = d.new_module("bar")?;
		let m_parent_async = m_parent.scope().new_signal("async")?.asynchronous().wire().build()?;

		let err = m_parent
			.scope()
			.new_module(m, "disappointing_child")?
			.bind("clk", m_parent_async)
			.build();

		assert!(matches!(err, Err(DesignError::IncompatibleBindingType { .. })));
		Ok(())
	}

	#[test]
	fn signal_add_eval_test() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let m = d.new_module("test")?;

		let a = m.scope().new_signal("a")?.unsigned(8u32.into()).constant().build()?;

		let b = m.scope().new_signal("b")?.unsigned(12u32.into()).constant().build()?;

		let expr = Expression::from(a) + b.into();

		let mut ctx = EvalContext::without_assumptions(d);
		ctx.assume(a, 14u32.into())?;
		ctx.assume(b, 16u32.into())?;

		let result = expr.eval(&ctx)?;
		assert_eq!(result.try_into_u64()?, 30u64);

		Ok(())
	}

	#[test]
	fn signal_array_eval_test() -> Result<(), DesignError> {
		let mut d = DesignHandle::new();
		let m = d.new_module("test")?;

		let a = m
			.scope()
			.new_signal("a")?
			.signed(8u32.into())
			.constant()
			.array(4u32.into())?
			.build()?;

		let mut ctx = EvalContext::without_assumptions(d);
		ctx.assume_array(a, vec![0], 1.into())?;
		ctx.assume_array(a, vec![1], 2.into())?;
		ctx.assume_array(a, vec![2], 15.into())?;
		ctx.assume_array(a, vec![3], 10.into())?;

		let result0 = Expression::Signal(a.index(0.into())).eval(&ctx)?;
		let result1 = Expression::Signal(a.index(1.into())).eval(&ctx)?;
		let result2 = Expression::Signal(a.index(2.into())).eval(&ctx)?;
		let result3 = Expression::Signal(a.index(3.into())).eval(&ctx)?;
		assert_eq!(result0.try_into_i64()?, 1.into());
		assert_eq!(result1.try_into_i64()?, 2.into());
		assert_eq!(result2.try_into_i64()?, 15.into());
		assert_eq!(result3.try_into_i64()?, 10.into());

		let index_expr = Expression::from(1) + Expression::from(2);
		let result = a.index(index_expr).eval(&ctx)?;
		assert_eq!(result.try_into_i64()?, 10.into());

		Ok(())
	}
}
