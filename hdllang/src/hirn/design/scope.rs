use std::collections::HashMap;
use log::debug;

use super::signal::{SignalType, Signal, SignalDirection, SignalBuilder};
use super::functional_blocks::BlockInstance;
use super::Expression;
use super::expression::NumericConstant;
use super::{ScopeRef, SignalRef, DesignError, DesignHandle};
use super::WeakDesignHandle;

pub struct ConditionalScope {
	pub condition: Expression,
	pub scope: ScopeRef,
}

pub struct RangeScope {
	pub iterator_var: SignalRef,
	pub iterator_begin: Expression,
	pub iterator_end: Expression,
	pub scope: ScopeRef,
}

pub struct Assignment {
	pub lhs: SignalRef,
	pub rhs: Expression,
}

pub struct Scope {
	pub(super) id: ScopeRef,
	parent: Option<ScopeRef>,
	assignments: Vec<Assignment>,
	loops: Vec<RangeScope>,
	conditionals: Vec<ConditionalScope>,
	blocks: Vec<BlockInstance>,
}



impl Scope {
	pub fn new() -> Self {
		Self {
			id: ScopeRef{id: 0},
			parent: None,
			assignments: vec![],
			loops: vec![],
			conditionals: vec![],
			blocks: vec![],
		}
	}
	
	fn set_parent(&mut self, parent: ScopeRef) -> Result<(), DesignError> {
		self.check_in_design()?;

		if self.parent.is_some() {
			return Err(DesignError::ScopeAlreadyOwned);
		}

		self.parent = Some(parent);
		Ok(())
	}

	fn check_in_design(&self) -> Result<(), DesignError> {
		if self.id.is_null() {
			return Err(DesignError::NotInDesign)
		}
		Ok(())
	}

	fn add_conditional_scope(&mut self, condition: Expression, child: ScopeRef) -> Result<ScopeRef, DesignError> {
		// self.add_subscope(child)?;
		self.conditionals.push(ConditionalScope{condition, scope: child});
		Ok(child)

		// TODO assert condition valid in this scope
		// TODO assert condition is compile-time constant
	}

	fn add_loop_scope(&mut self, iterator_var: SignalRef, iterator_begin: Expression, iterator_end: Expression, child: ScopeRef) -> Result<ScopeRef, DesignError> {
		// self.add_subscope(child)?;
		self.loops.push(RangeScope{iterator_var, iterator_begin, iterator_end, scope: child});
		Ok(child)

		// TODO assert iterator var in child scope
		// TODO assert expressions valid in this scope
		// TODO assert iterator_var type is int
		// TODO assert iterator_begin is compile-time constant
		// TODO assert iterator_end is compile-time constant
	}

	fn assign_signal(&mut self, signal: SignalRef, expr: Expression) -> Result<(), DesignError> {
		self.assignments.push(Assignment{lhs: signal, rhs: expr});
		Ok(())

		// TODO assert signal accessible from this scope
		// TODO expression valid in this scope
	}

	

}

pub struct ScopeHandle {
	design: DesignHandle,
	scope: ScopeRef,
}


macro_rules! this_scope {
	($self:ident) => {
		$self.design.borrow_mut().get_scope_mut($self.scope).unwrap()
	}
}

impl ScopeHandle {
	pub fn id(&self) -> ScopeRef {
		self.scope
	}

	pub fn new(design: DesignHandle, scope: ScopeRef) -> Self {
		Self {
			design,
			scope,
		}
	}

	fn new_child_scope(&mut self) -> ScopeHandle {
		let child = self.design.borrow_mut().new_scope();
		this_scope!(self).set_parent(self.scope).unwrap();
		child
	}

	pub fn new_subscope(&mut self) -> ScopeHandle {
		self.new_child_scope()
	}

	pub fn if_scope(&mut self, condition: Expression) -> Result<ScopeHandle, DesignError> {
		let child = self.new_child_scope();
		this_scope!(self).add_conditional_scope(condition, child.id()).unwrap();
		Ok(child)
	}

	pub fn new_signal(&mut self) -> Result<SignalBuilder, DesignError> {
		Ok(SignalBuilder::new(self.design.clone(), self.scope))
	}
}