use super::functional_blocks::{BlockInstance, ModuleInstanceBuilder};
use super::signal::SignalBuilder;
use super::{DesignError, DesignHandle, ModuleId, RegisterBuilder, ScopeId, SignalId};
use super::{Expression, ModuleHandle};

/// Scope associated with an if statement
#[derive(Clone, Debug)]
pub struct ConditionalScope {
	/// Condition expression
	pub condition: Expression,

	/// Child scope
	pub scope: ScopeId,
}

/// Scope associated with a loop statement
#[derive(Clone, Debug)]
pub struct RangeScope {
	/// Iterator variable
	pub iterator_var: SignalId,

	/// Iterator begin expression
	pub iterator_begin: Expression,

	/// Iterator end expression
	pub iterator_end: Expression,

	/// Child scope
	pub scope: ScopeId,
}

/// Assignment of signals
#[derive(Clone, Debug)]
pub struct Assignment {
	/// Left-hand side of the assignment
	pub lhs: Expression,

	/// Right-hand side of the assignment (an expression)
	pub rhs: Expression,
}

/// Scope representation
pub struct Scope {
	/// Self-reference
	pub(super) id: ScopeId,

	/// Parent scope (optional)
	parent: Option<ScopeId>,

	/// Parent module
	module: ModuleId,

	/// Assignments made inside this scope
	assignments: Vec<Assignment>,

	/// Loops inside this scope
	loops: Vec<RangeScope>,

	/// Conditionals inside this scope
	conditionals: Vec<ConditionalScope>,

	/// Blocks instantiated inside this scope
	blocks: Vec<BlockInstance>,
}

impl Scope {
	/// Creates a new scope
	pub(super) fn new(id: ScopeId, module: ModuleId, parent_scope: Option<ScopeId>) -> Self {
		Self {
			id,
			module,
			parent: parent_scope,
			assignments: vec![],
			loops: vec![],
			conditionals: vec![],
			blocks: vec![],
		}
	}

	/// Checks if this scope is in a design
	fn check_in_design(&self) -> Result<(), DesignError> {
		if self.id.is_null() {
			return Err(DesignError::NotInDesign);
		}
		Ok(())
	}

	/// Adds a new conditional sub-scope
	fn add_conditional_scope(&mut self, condition: Expression, child: ScopeId) -> Result<ScopeId, DesignError> {
		// self.add_subscope(child)?;
		self.conditionals.push(ConditionalScope {
			condition,
			scope: child,
		});
		Ok(child)

		// TODO assert condition valid in this scope
		// TODO assert condition is compile-time constant
	}

	/// Adds a new loop sub-scope
	fn add_loop_scope(
		&mut self,
		iterator_var: SignalId,
		iterator_begin: Expression,
		iterator_end: Expression,
		child: ScopeId,
	) -> Result<ScopeId, DesignError> {
		// self.add_subscope(child)?;
		self.loops.push(RangeScope {
			iterator_var,
			iterator_begin,
			iterator_end,
			scope: child,
		});
		Ok(child)

		// TODO assert iterator var in child scope
		// TODO assert expressions valid in this scope
		// TODO assert iterator_var type is int
		// TODO assert iterator_begin is compile-time constant
		// TODO assert iterator_end is compile-time constant
	}

	/// Assigns to a signal in this scope
	fn assign_signal(&mut self, signal: Expression, expr: Expression) -> Result<(), DesignError> {
		self.assignments.push(Assignment { lhs: signal, rhs: expr });
		Ok(())

		// TODO assert signal accessible from this scope
		// TODO expression valid in this scope
	}

	/// Adds a block instance in this scope
	fn add_block(&mut self, block: BlockInstance) -> Result<(), DesignError> {
		self.blocks.push(block);

		// TODO check if all expressions can be evaluated inside this scope

		Ok(())
	}
}

/// Handle used for manipulating scopes outside of the design
#[derive(Clone)]
pub struct ScopeHandle {
	/// Handle to the design
	design: DesignHandle,

	/// ID of the scope
	scope: ScopeId,
}

/// A helper macro for getting a mutable reference to the scope in the ScopeHandle
macro_rules! this_scope {
	($self:ident) => {
		$self.design.borrow_mut().get_scope_mut($self.scope).unwrap()
	};
}

impl ScopeHandle {
	/// Returns the ID of the scope
	pub fn id(&self) -> ScopeId {
		self.scope
	}

	pub fn design(&self) -> DesignHandle {
		self.design.clone()
	}

	/// Creates a new scope handle
	pub fn new(design: DesignHandle, scope: ScopeId) -> Self {
		Self { design, scope }
	}

	/// Creates a scope and sets its parent to this scope
	pub fn new_subscope(&mut self) -> Result<ScopeHandle, DesignError> {
		let module = this_scope!(self).module;
		let child = self.design.borrow_mut().new_scope(module, Some(self.scope));
		child
	}

	/// Creates a new if statement in this scope
	pub fn if_scope(&mut self, condition: Expression) -> Result<ScopeHandle, DesignError> {
		let child = self.new_subscope()?;
		this_scope!(self).add_conditional_scope(condition, child.id()).unwrap();
		Ok(child)
	}

	/// Creates a new loop statement in this scope
	/// The iterator variable is automatically defined and returned
	pub fn loop_scope(
		&mut self,
		iter_name: &str,
		from: Expression,
		to: Expression,
	) -> Result<(ScopeHandle, SignalId), DesignError> {
		let mut child = self.new_subscope()?;

		let iter_var = child.new_signal()?
			.name(iter_name)
			.signed(Expression::new_one()) // TODO signed 64-bit
			.constant()
			.build()?;

		this_scope!(self).add_loop_scope(iter_var, from, to, child.id())?;

		Ok((child, iter_var))
	}

	/// Returns a new register builder
	pub fn new_register(&mut self) -> Result<RegisterBuilder, DesignError> {
		Ok(RegisterBuilder::new(self.clone()))
	}

	/// Adds a functional block
	pub(super) fn add_block(&mut self, block: BlockInstance) -> Result<(), DesignError> {
		this_scope!(self).add_block(block)
	}

	/// Assigns an expression to a signal
	pub fn assign(&mut self, signal: Expression, expr: Expression) -> Result<(), DesignError> {
		this_scope!(self).assign_signal(signal, expr)
	}

	/// Creates a new signal in this scope (returns a builder)
	pub fn new_signal(&mut self) -> Result<SignalBuilder, DesignError> {
		Ok(SignalBuilder::new(self.design.clone(), self.scope))
	}

	/// Creates a new module instance in this scope (returns a builder)
	pub fn new_module(&mut self, module: ModuleHandle) -> Result<ModuleInstanceBuilder, DesignError> {
		Ok(ModuleInstanceBuilder::new(self.clone(), module))
	}

	pub fn assignments(&self) -> Vec<Assignment> {
		this_scope!(self).assignments.clone()
	}

	pub fn signals(&self) -> Vec<SignalId> {
		self.design.borrow().get_scope_signals(self.scope).cloned().unwrap_or(vec![])
	}

	pub fn subscopes(&self) -> Vec<ScopeId> {
		self.design.borrow().get_scope_scopes(self.scope).cloned().unwrap_or(vec![])
	}

	pub fn conditional_subscopes(&self) -> Vec<ConditionalScope> {
		this_scope!(self).conditionals.clone()
	}

	pub fn loop_subscopes(&self) -> Vec<RangeScope> {
		this_scope!(self).loops.clone()
	}

	pub fn blocks(&self) -> Vec<BlockInstance> {
		this_scope!(self).blocks.clone()
	}
}
