use std::collections::{HashMap, HashSet};

use log::error;

use super::functional_blocks::{BlockInstance, ModuleInstanceBuilder};
use super::signal::{SignalBuilder, SignalSliceRange};
use super::{
	DesignError, DesignHandle, EvalContext, Evaluates, EvaluatesType, HasComment, HasSensitivity, HasSignedness,
	ModuleId, RegisterBuilder, ScopeId, SignalId, WidthExpression,
};
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

impl ConditionalScope {
	/// Returns a set of variables on which the condition of this scope depends
	pub fn condition_dependencies(&self) -> HashSet<SignalId> {
		self.condition.get_variables()
	}
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

impl RangeScope {
	/// Returns a set of variables on which the condition of this scope depends
	pub fn condition_dependencies(&self) -> HashSet<SignalId> {
		let mut deps = HashSet::new();
		deps.extend(self.iterator_begin.get_variables().iter());
		deps.extend(self.iterator_end.get_variables().iter());
		deps
	}
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

	/// Returns a set of variables which need to be evaluated before making the assignment
	pub fn dependencies(&self) -> HashSet<SignalId> {
		let mut deps = HashSet::new();
		deps.extend(self.rhs.get_variables().iter());
		let lhs_slice = self.lhs.try_drive().expect("LHS not drivable in assignment");
		for index in lhs_slice.indices {
			deps.extend(index.get_variables().iter());
		}
		deps
	}

	pub fn dependencies_bits(&self) -> Vec<SignalSliceRange> {
		let mut deps = vec![];
		deps.extend(self.rhs.get_used_slice_ranges());
		let lhs_slice = self.lhs.try_drive().expect("LHS not drivable in assignment");
		for index in lhs_slice.indices {
			deps.extend(index.get_used_slice_ranges());
		}
		deps
	}

	/// Returns ID of the signal driven by this assignment
	pub fn driven_signal(&self) -> SignalId {
		self.lhs.try_drive().expect("LHS not drivable in assignment").signal
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
	pub(super) _id: ScopeId,

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

	/// Unused expressions
	unused: Vec<Expression>,
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
			_id: id,
			module,
			parent: parent_scope,
			assignments: vec![],
			loops: vec![],
			conditionals: vec![],
			blocks: vec![],
			comment: None,
			unused: vec![],
		}
	}

	/// Adds a new conditional sub-scope
	fn add_conditional_scope(
		&mut self,
		condition: Expression,
		child: ScopeId,
		else_scope: Option<ScopeId>,
	) -> Result<ScopeId, DesignError> {
		self.conditionals.push(ConditionalScope {
			condition,
			scope: child,
			else_scope,
		});
		Ok(child)
	}

	/// Adds a new loop sub-scope
	fn add_loop_scope(
		&mut self,
		iterator_var: SignalId,
		iterator_begin: Expression,
		iterator_end: Expression,
		child: ScopeId,
	) -> Result<ScopeId, DesignError> {
		self.loops.push(RangeScope {
			iterator_var,
			iterator_begin,
			iterator_end,
			scope: child,
		});
		Ok(child)
	}

	/// Adds a block instance in this scope
	fn add_block(&mut self, block: BlockInstance) -> Result<(), DesignError> {
		self.blocks.push(block);
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

	/// Validates if condition to be generic boolean
	fn validate_if_condition(&self, condition: &Expression) -> Result<(), DesignError> {
		let eval_ctx = EvalContext::without_assumptions(self.design());
		condition.validate(&eval_ctx, self)?;
		let cond_type = condition.eval_type(&eval_ctx)?;
		if !cond_type.is_generic() || !cond_type.is_unsigned() {
			return Err(DesignError::InvalidIfCondition);
		}

		match condition.width()?.try_eval_ignore_missing_into::<i64>(&eval_ctx)? {
			Some(1) => {},
			Some(_) => return Err(DesignError::InvalidIfCondition),
			None => {},
		}

		Ok(())
	}

	/// Creates a new if statement in this scope
	pub fn if_scope(&mut self, condition: Expression) -> Result<ScopeHandle, DesignError> {
		self.validate_if_condition(&condition)?;
		let child = self.new_subscope()?;
		this_scope!(self)
			.add_conditional_scope(condition, child.id(), None)
			.unwrap();
		Ok(child)
	}

	/// Creates a new if statement with else clause in this scope
	pub fn if_else_scope(&mut self, condition: Expression) -> Result<(ScopeHandle, ScopeHandle), DesignError> {
		self.validate_if_condition(&condition)?;
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
		let eval_ctx = EvalContext::without_assumptions(self.design());
		from.validate(&eval_ctx, self)?;
		to.validate(&eval_ctx, self)?;
		let from_type = from.eval_type(&eval_ctx)?;
		let to_type = from.eval_type(&eval_ctx)?;
		if !from_type.is_generic() || !to_type.is_generic() || !from_type.is_signed() || !to_type.is_signed() {
			return Err(DesignError::InvalidLoopRange);
		}

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
		lhs.validate_no_assumptions(self)?;
		rhs.validate_no_assumptions(self)?;

		// Check if the LHS is drivable
		lhs.try_drive().ok_or(DesignError::ExpressionNotDrivable)?;

		// Validate types (not clock-aware)
		let lhs_type = lhs.eval_type(&EvalContext::without_assumptions(self.design()))?;
		let rhs_type = rhs.eval_type(&EvalContext::without_assumptions(self.design()))?;

		if lhs_type.is_signed() != rhs_type.is_signed() {
			return Err(DesignError::IncompatibleSignedness {
				lhs_signedness: lhs_type.signedness(),
				rhs_signedness: rhs_type.signedness(),
			});
		}

		if !rhs_type.sensitivity().can_drive(lhs_type.sensitivity()) {
			error!("Assigning {:?} = {:?}", lhs, rhs);
			error!("Assigning {:?} = {:?}", lhs_type, rhs_type);
			return Err(DesignError::IncompatibleSensitivity {
				lhs_sensitivity: lhs_type.sensitivity().clone(),
				rhs_sensitivity: rhs_type.sensitivity().clone(),
			});
		}

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
	pub fn assign_with_comment(&mut self, lhs: Expression, rhs: Expression, comment: &str) -> Result<(), DesignError> {
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

	/// Marks a drivable expression as intentionally unused
	pub fn mark_unused(&mut self, expr: Expression) -> Result<(), DesignError> {
		expr.validate_no_assumptions(self)?;
		expr.try_drive().ok_or(DesignError::ExpressionNotDrivable)?;
		this_scope!(self).unused.push(expr);
		Ok(())
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

	pub fn unused_expressions(&self) -> Vec<Expression> {
		this_scope!(self).unused.clone()
	}

	pub fn parent(&self) -> Option<ScopeId> {
		this_scope!(self).parent
	}

	pub fn parent_handle(&self) -> Option<ScopeHandle> {
		self.parent().map(|p| self.design.borrow().get_scope_handle(p).unwrap())
	}

	pub fn visible_signals(&self) -> HashSet<SignalId> {
		let mut signals: HashSet<SignalId> = self.signals().into_iter().collect();

		if let Some(parent) = self.parent_handle() {
			let ext_signals = parent.visible_signals();

			// Create a mapping between external signal names and their IDs
			let ext_sig_names: HashMap<String, SignalId> = ext_signals
				.iter()
				.map(|s| {
					let design = self.design.borrow();
					let name = design.get_signal(*s).unwrap().name().to_string();
					(name, *s)
				})
				.collect();

			// Internal signal names
			let int_sig_names: HashSet<String> = signals
				.iter()
				.map(|s| {
					let design = self.design.borrow();
					design.get_signal(*s).unwrap().name().to_string()
				})
				.collect();

			// We select signals with names that overlap with names of signals
			// defined directly in this scope
			let shadowed_signals: HashSet<SignalId> = int_sig_names
				.iter()
				.filter_map(|name| ext_sig_names.get(name).copied())
				.collect();

			signals.extend(ext_signals.difference(&shadowed_signals));
		}
		signals
	}
}

impl std::fmt::Debug for ScopeHandle {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", this_scope!(self))
	}
}
