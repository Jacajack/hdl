pub mod eval;
pub mod expression;
pub mod expression_eval;
pub mod expression_ops;
pub mod functional_blocks;
pub mod module;
pub mod numeric_constant;
pub mod scope;
pub mod signal;
pub mod utils;

pub use eval::{EvalContext, EvalError};
pub use expression::{BinaryOp, Expression, UnaryOp};
pub use functional_blocks::{Register, RegisterBuilder};
pub use module::{InterfaceSignal, Module, ModuleHandle, SignalDirection};
pub use numeric_constant::NumericConstant;
pub use scope::{Scope, ScopeHandle};
pub use signal::{Signal, SignalClass, SignalSensitivity, SignalSignedness, SignalSlice};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use thiserror::Error;

use self::eval::EvalType;

/// References a module in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
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
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
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
			signal: self.clone(),
			indices: vec![index],
		}
	}
}

/// References a scope in a design
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
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
	weak: WeakDesignHandle,
	modules: Vec<Module>,
	scopes: Vec<Scope>,
	signals: Vec<Signal>,
	next_module_id: usize,
	next_scope_id: usize,
	next_signal_id: usize,
	scope_signals: HashMap<ScopeId, Vec<SignalId>>,
	module_scopes: HashMap<ModuleId, Vec<ScopeId>>,
}

impl DesignCore {
	/// Creates a new empty design
	pub fn new() -> Self {
		Self {
			weak: WeakDesignHandle::new(),
			modules: Vec::new(),
			scopes: Vec::new(),
			signals: Vec::new(),
			next_module_id: 1,
			next_scope_id: 1,
			next_signal_id: 1,
			scope_signals: HashMap::new(),
			module_scopes: HashMap::new(),
		}
	}

	/// Creates a new scope and adds it to the design
	fn new_scope(&mut self, module: ModuleId) -> Result<ScopeHandle, DesignError> {
		let id = ScopeId { id: self.next_scope_id };
		self.next_scope_id += 1;

		let scope = Scope::new(id, module);
		self.scopes.push(scope);
		self.module_scopes.entry(module).or_insert(vec![]).push(id);
		Ok(ScopeHandle::new(self.weak.upgrade().unwrap(), id))
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
				return Err(DesignError::SignalNameConflict {
					scope: sig.parent_scope,
					first: other.id,
					second: sig.id,
				});
			}
		}

		// Add to design
		self.scope_signals
			.entry(sig.parent_scope)
			.or_insert(vec![])
			.push(SignalId { id });
		self.signals.push(sig);
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
				return Err(DesignError::ModuleNameConflict {
					first: other.id,
					second: m.id,
				});
			}
		}

		self.modules.push(m);
		Ok(ModuleHandle::new(self.weak.upgrade().unwrap(), ModuleId { id }))
	}

	/// Returns a mutable reference to the signal with the given ID
	fn get_scope_mut(&mut self, scope: ScopeId) -> Option<&mut Scope> {
		self.scopes.get_mut(scope.id - 1)
	}

	/// Returns a reference to the scope with the given ID
	fn get_scope(&self, scope: ScopeId) -> Option<&Scope> {
		self.scopes.get(scope.id - 1)
	}

	/// Returns a handle to the scope with the given ID
	fn get_scope_handle(&self, scope: ScopeId) -> Option<ScopeHandle> {
		Some(ScopeHandle::new(self.weak.upgrade()?, scope))
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
		Some(ModuleHandle::new(self.weak.upgrade()?, module))
	}

	/// Returns a reference to the signal with the given ID
	fn get_signal(&self, signal: SignalId) -> Option<&Signal> {
		self.signals.get(signal.id - 1)
	}

	/// Returns a list of signals in the given scope
	fn get_scope_signals(&self, scope: ScopeId) -> Option<&Vec<SignalId>> {
		self.scope_signals.get(&scope)
	}

	/// Creates a new module in the design
	pub fn new_module(&mut self, name: &str) -> Result<ModuleHandle, DesignError> {
		let module = Module::new(name, vec![])?;
		let handle = self.add_module(module)?;
		let main_scope = self.new_scope(handle.id())?;
		self.get_module_mut(handle.id()).unwrap().main_scope = main_scope.id();
		Ok(handle)
	}
}

/// Weak reference to a design
pub type WeakDesignHandle = Weak<RefCell<DesignCore>>;

/// Strong reference to a design
pub type DesignHandle = Rc<RefCell<DesignCore>>;

/// Represents a hardware design
pub struct Design {
	handle: DesignHandle,
}

impl Design {
	/// Creates a new HIRN design
	pub fn new() -> Self {
		let d = Self {
			handle: Rc::new(RefCell::new(DesignCore::new())),
		};

		d.handle.borrow_mut().weak = Rc::downgrade(&d.handle);
		d
	}

	fn borrow_mut(&mut self) -> std::cell::RefMut<DesignCore> {
		self.handle.borrow_mut()
	}

	fn borrow(&self) -> std::cell::Ref<DesignCore> {
		self.handle.borrow()
	}

	pub fn handle(&self) -> DesignHandle {
		self.handle.clone()
	}

	/// Creates a new module with provided name and returns a handle to it
	pub fn new_module(&mut self, name: &str) -> Result<ModuleHandle, DesignError> {
		self.handle.borrow_mut().new_module(name)
	}

	pub fn get_module_handle(&self, module: ModuleId) -> Option<ModuleHandle> {
		self.handle.borrow().get_module_handle(module)
	}

	pub fn get_signal(&self, signal: SignalId) -> Option<Signal> {
		self.handle.borrow().get_signal(signal).cloned()
	}
}

/// Represents an error that can occur during design construction.
/// Elaboration errors are not accounted for here.
#[derive(Clone, Debug, Error)]
pub enum DesignError {
	#[error("This object is not part of any HIRN Design.")]
	NotInDesign,

	#[error("Provided scope is not part of any HIRN design.")]
	ScopeNotInDesign,

	#[error("Provided scope already has a parent scope assigned.")]
	ScopeAlreadyOwned,

	#[error("Invalid name")]
	InvalidName,

	#[error("Invalid module ID")]
	InvalidModuleId(ModuleId),

	#[error("Duplicate module interface binding")]
	DuplicateInterfaceBinding(ModuleId),

	#[error("Invalid interface signal name")]
	InvalidInterfaceSignalName(ModuleId),

	#[error("Signal width not specified")]
	SignalWidthNotSpecified,

	#[error("Signal class not specified")]
	SignalClassNotSpecified,

	#[error("Cannot evaluate width of numeric constant without making assumptions")]
	CannotEvaluateNumericConstantWidth(EvalError),

	#[error("Numeric constant width evaluated to a large, unsupported value")]
	NumericConstantWidthTooLarge,

	#[error("Numeric constant width is not sufficient to store the provided value")]
	NumericConstantWidthTooSmall,

	#[error(transparent)]
	EvalError(#[from] EvalError),

	#[error("Expression cannot be driven - cannot bind to an output or use in assignment LHS")]
	ExpressionNotDriveable, // TODO more details

	#[error("Signal sensitivity not specified")]
	SignalSensitivityNotSpecified,

	#[error("Conflicting signal sensitivity")]
	ConflictingSignalSensitivity,

	#[error("Required register signal is not connected")]
	RequiredRegisterSignalNotConnected(functional_blocks::ReqiuredRegisterSignal),

	#[error("Signal name conflict in scope")]
	SignalNameConflict {
		scope: ScopeId,
		first: SignalId,
		second: SignalId,
	},

	#[error("Module name conflict")]
	ModuleNameConflict { first: ModuleId, second: ModuleId },

	#[error("Incompatible types in binding")]
	IncompatibleBindingType {
		module: ModuleId,
		signal: SignalId,
		binding_type: EvalType,
		interface_type: EvalType,
	},
}

#[cfg(test)]
mod test {
	use super::{eval::Evaluates, *};

	#[test]
	pub fn design_basic_test() -> Result<(), DesignError> {
		// init();
		let mut d = Design::new();
		let mut m = d.new_module("test")?;

		let sig = m
			.scope()
			.new_signal()?
			.name("test_signal")
			.unsigned(Expression::new_zero())
			.constant()
			.build()?;

		let expr = Expression::from(sig) + sig.into();

		let sig2 = m
			.scope()
			.new_signal()?
			.name("test_signal_2")
			.unsigned(expr.clone())
			.constant()
			.build()?;

		let mut scope2 = m.scope().if_scope(Expression::new_zero())?;

		scope2
			.new_register()?
			.clk(sig.into())
			.nreset(expr.clone())
			.next(Expression::new_zero())
			.output(sig2.into())
			.build()?;

		scope2.assign(sig2.into(), expr)?;

		Ok(())
	}

	/// Verifies if the design correctly detects signal name conflicts
	#[test]
	fn test_unique_signal_names() -> Result<(), DesignError> {
		let mut d = Design::new();
		let mut m = d.new_module("test")?;

		let _sig = m
			.scope()
			.new_signal()?
			.name("name")
			.unsigned(Expression::new_one())
			.constant()
			.build()?;

		let sig2 = m
			.scope()
			.new_signal()?
			.name("name")
			.unsigned(Expression::new_one())
			.constant()
			.build();

		assert!(matches!(sig2, Err(DesignError::SignalNameConflict { .. })));
		Ok(())
	}

	/// Verifies if the design enforces unique module names
	#[test]
	fn test_unique_module_names() -> Result<(), DesignError> {
		let mut d = Design::new();
		let _m = d.new_module("name")?;
		let m2 = d.new_module("name");

		assert!(matches!(m2, Err(DesignError::ModuleNameConflict { .. })));
		Ok(())
	}

	/// Verify module naming rules
	#[test]
	fn test_module_naming_rules() -> Result<(), DesignError> {
		let mut d = Design::new();
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
		let mut d = Design::new();
		let mut m = d.new_module("foo")?;

		let clk = m
			.scope()
			.new_signal()?
			.name("clk")
			.clock()
			.unsigned(Expression::new_one())
			.build()?;

		let nreset = m
			.scope()
			.new_signal()?
			.name("nreset")
			.asynchronous()
			.unsigned(Expression::new_one())
			.build()?;

		let next = m
			.scope()
			.new_signal()?
			.name("next")
			.unsigned(Expression::new_one())
			.comb(clk, true)
			.build()?;

		let output = m
			.scope()
			.new_signal()?
			.name("output")
			.unsigned(Expression::new_one())
			.sync(clk, true)
			.build()?;

		let _register = m
			.scope()
			.new_register()?
			.clk(clk.into())
			.nreset(nreset.into())
			.next(next.into())
			.output(output.into())
			.build()?;

		Ok(())
	}

	/// Test module interface binding
	#[test]
	fn test_interfaces() -> Result<(), DesignError> {
		let mut d = Design::new();
		let mut m = d.new_module("foo")?;
		let m_clk = m.scope().new_signal()?.name("clk").clock().wire().build()?;

		m.expose(m_clk, SignalDirection::Input)?;

		let mut m_parent = d.new_module("bar")?;
		let m_parent_clk = m_parent.scope().new_signal()?.name("clk").clock().wire().build()?;

		m_parent
			.scope()
			.new_module(m)?
			.bind("clk", m_parent_clk.into())
			.build()?;

		Ok(())
	}

	/// Test invalid module interface binding async->clk
	#[test]
	fn test_bad_interface_binding() -> Result<(), DesignError> {
		let mut d = Design::new();
		let mut m = d.new_module("foo")?;
		let m_clk = m.scope().new_signal()?.name("clk").clock().wire().build()?;

		m.expose(m_clk, SignalDirection::Input)?;

		let mut m_parent = d.new_module("bar")?;
		let m_parent_async = m_parent
			.scope()
			.new_signal()?
			.name("async")
			.asynchronous()
			.wire()
			.build()?;

		let err = m_parent
			.scope()
			.new_module(m)?
			.bind("clk", m_parent_async.into())
			.build();

		assert!(matches!(err, Err(DesignError::IncompatibleBindingType { .. })));
		Ok(())
	}

	#[test]
	fn signal_add_eval_test() -> Result<(), DesignError> {
		let mut d = Design::new();
		let mut m = d.new_module("test")?;

		let a = m
			.scope()
			.new_signal()?
			.name("a")
			.unsigned(8.into())
			.constant()
			.build()?;

		let b = m
			.scope()
			.new_signal()?
			.name("b")
			.unsigned(12.into())
			.constant()
			.build()?;

		let expr = Expression::from(a) + b.into();

		let mut ctx = EvalContext::without_assumptions(d.handle());
		ctx.assume_scalar(a, 14.into());
		ctx.assume_scalar(b, 16.into());

		let result = expr.eval(&ctx)?;
		assert_eq!(result.try_into_u64(), 30.into());

		Ok(())
	}

	#[test]
	fn signal_array_eval_test() -> Result<(), DesignError> {
		let mut d = Design::new();
		let mut m = d.new_module("test")?;

		let a = m
			.scope()
			.new_signal()?
			.name("a")
			.unsigned(8.into())
			.constant()
			.array(4.into())?
			.build()?;

		let mut ctx = EvalContext::without_assumptions(d.handle());
		ctx.assume(a, vec![0], 1.into());
		ctx.assume(a, vec![1], 2.into());
		ctx.assume(a, vec![2], 15.into());
		ctx.assume(a, vec![3], 10.into());

		let result0 = Expression::Signal(a.index(0.into())).eval(&ctx)?;
		let result1 = Expression::Signal(a.index(1.into())).eval(&ctx)?;
		let result2 = Expression::Signal(a.index(2.into())).eval(&ctx)?;
		let result3 = Expression::Signal(a.index(3.into())).eval(&ctx)?;
		assert_eq!(result0.try_into_i64(), 1.into());
		assert_eq!(result1.try_into_i64(), 2.into());
		assert_eq!(result2.try_into_i64(), 15.into());
		assert_eq!(result3.try_into_i64(), 10.into());

		let index_expr = Expression::from(1) + Expression::from(2);
		let result = a.index(index_expr).eval(&ctx)?;
		assert_eq!(result.try_into_i64(), 10.into());

		Ok(())
	}
}
