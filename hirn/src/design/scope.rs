use std::collections::HashSet;

use super::functional_blocks::{BlockInstance, ModuleInstanceBuilder};
use super::signal::SignalBuilder;
use super::{DesignError, DesignHandle, HasComment, ModuleId, RegisterBuilder, ScopeId, SignalId, EvalContext};
use super::{Expression, ModuleHandle};

/// Scope associated with an if statement
#[derive(Clone, Debug)]
pub struct ConditionalScope {
	/// Condition expression
	pub condition: Expression,

	/// Child scope
	pub scope: ScopeId,

	/// Child else scope
	pub else_scope: Option<ScopeId>,
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

	/// Source-code comment
	comment: Option<String>,
}

impl Assignment {
	/// Creates a new assignment
	pub fn new(lhs: Expression, rhs: Expression) -> Self {
		Self {
			lhs,
			rhs,
			comment: None,
		}
	}

	/// New assignment with commenty
	pub fn with_comment(lhs: Expression, rhs: Expression, comment: &str) -> Self {
		let mut a = Self::new(lhs, rhs);
		a.comment(comment);
		a
	}

	/// Sets the comment
	pub fn comment(&mut self, comment: &str) {
		self.comment = Some(comment.to_string());
	}
}

impl HasComment for Assignment {
	fn get_comment(&self) -> Option<String> {
		self.comment.clone()
	}
}

/// Scope representation
#[derive(Debug)]
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

	/// Comment for the entire scope
	comment: Option<String>,
}

impl HasComment for Scope {
	fn get_comment(&self) -> Option<String> {
		self.comment.clone()
	}
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
			comment: None,
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
	fn add_conditional_scope(
		&mut self,
		condition: Expression,
		child: ScopeId,
		else_scope: Option<ScopeId>,
	) -> Result<ScopeId, DesignError> {
		// self.add_subscope(child)?;
		self.conditionals.push(ConditionalScope {
			condition,
			scope: child,
			else_scope,
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

	/// Adds a block instance in this scope
	fn add_block(&mut self, block: BlockInstance) -> Result<(), DesignError> {
		self.blocks.push(block);

		// TODO check if all expressions can be evaluated inside this scope

		Ok(())
	}

	/// Sets the comment
	fn comment(&mut self, comment: &str) {
		self.comment = Some(comment.to_string());
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

impl HasComment for ScopeHandle {
	fn get_comment(&self) -> Option<String> {
		this_scope!(self).get_comment()
	}
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
		this_scope!(self)
			.add_conditional_scope(condition, child.id(), None)
			.unwrap();
		Ok(child)
	}

	/// Creates a new if statement with else clause in this scope
	pub fn if_else_scope(&mut self, condition: Expression) -> Result<(ScopeHandle, ScopeHandle), DesignError> {
		let child = self.new_subscope()?;
		let else_scope = self.new_subscope()?;
		this_scope!(self)
			.add_conditional_scope(condition, child.id(), Some(else_scope.id()))
			.unwrap();
		Ok((child, else_scope))
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

		let iter_var = child.new_signal(iter_name)?
			.signed(64.into()) // TODO is that right? Maybe we want semantic analyzer to provide that signal and not create it here???
			.generic()
			.build()?;

		this_scope!(self).add_loop_scope(iter_var, from, to, child.id())?;

		Ok((child, iter_var))
	}

	/// Returns a new register builder
	pub fn new_register(&mut self, name: &str) -> Result<RegisterBuilder, DesignError> {
		Ok(RegisterBuilder::new(self.clone(), name))
	}

	/// Adds a functional block
	pub(super) fn add_block(&mut self, block: BlockInstance) -> Result<(), DesignError> {
		this_scope!(self).add_block(block)
	}

	fn assign_impl(&mut self, lhs: Expression, rhs: Expression, comment: Option<&str>) -> Result<(), DesignError> {
		lhs.validate_no_assumptions(&self)?;
		rhs.validate_no_assumptions(&self)?;
		// TODO assert no indexing if LHS is generic
		// TODO assert LHS drivable
		// TODO assert that scope is unconditional relative to the declaration if LSH is generic


		// Save the assignment
		this_scope!(self).assignments.push(Assignment::new(lhs, rhs));

		// Optionally: comment the assignment
		if let Some(comment) = comment {
			this_scope!(self).assignments.last_mut().unwrap().comment(comment);
		}

		Ok(())	
	}

	/// Assigns an expression to a drivable expression
	pub fn assign(&mut self, lhs: Expression, rhs: Expression) -> Result<(), DesignError> {
		self.assign_impl(lhs, rhs, None)
	}

	/// Assigns an expression to a drivable expression and adds a comment
	pub fn assign_with_comment(
		&mut self,
		lhs: Expression,
		rhs: Expression,
		comment: &str,
	) -> Result<(), DesignError> {
		self.assign_impl(lhs, rhs, Some(comment))
	}

	/// Creates a new signal in this scope (returns a builder)
	pub fn new_signal(&mut self, name: &str) -> Result<SignalBuilder, DesignError> {
		Ok(SignalBuilder::new(self.design.clone(), self.scope, name))
	}

	/// Creates a new module instance in this scope (returns a builder)
	pub fn new_module(&mut self, module: ModuleHandle, name: &str) -> Result<ModuleInstanceBuilder, DesignError> {
		Ok(ModuleInstanceBuilder::new(self.clone(), module, name))
	}

	/// Sets comment for the scope
	pub fn comment(&mut self, comment: &str) {
		this_scope!(self).comment(comment);
	}

	pub fn assignments(&self) -> Vec<Assignment> {
		this_scope!(self).assignments.clone()
	}

	pub fn signals(&self) -> Vec<SignalId> {
		self.design
			.borrow()
			.get_scope_signals(self.scope)
			.cloned()
			.unwrap_or(vec![])
	}

	pub fn subscopes(&self) -> Vec<ScopeId> {
		self.design
			.borrow()
			.get_scope_scopes(self.scope)
			.cloned()
			.unwrap_or(vec![])
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

	pub fn parent(&self) -> Option<ScopeId> {
		this_scope!(self).parent
	}

	pub fn parent_handle(&self) -> Option<ScopeHandle> {
		self.parent().map(|p| self.design.borrow().get_scope_handle(p).unwrap())
	}

	pub fn visible_signals(&self) -> HashSet<SignalId> {
		let mut signals: HashSet<SignalId> = self.signals().into_iter().collect();

		// FIXME we need to account for shadowing here!

		if let Some(parent) = self.parent_handle() {
			signals.extend(parent.visible_signals());
		}
		signals
	}
}

impl std::fmt::Debug for ScopeHandle {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", this_scope!(self))
	}
}
