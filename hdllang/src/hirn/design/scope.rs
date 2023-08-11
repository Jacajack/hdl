use std::collections::HashMap;
use super::signal::{SignalType, Signal, SignalDirection, SignalBuilder};
use super::functional_blocks::BlockInstance;
use super::Expression;
use super::expression::NumericConstant;
use super::{ScopeRef, SignalRef, DesignError};
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
	pub(super) design: WeakDesignHandle,
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
			design: WeakDesignHandle::new(),
			id: ScopeRef{id: 0},
			parent: None,
			assignments: vec![],
			loops: vec![],
			conditionals: vec![],
			blocks: vec![],
		}
	}

	pub(super) fn set_design(&mut self, design: WeakDesignHandle, id: usize) {
		assert!(self.id.is_null());
		self.design = design;
		self.id = ScopeRef{id};
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

	pub fn add_subscope(&mut self, child_ref: ScopeRef) -> Result<(), DesignError> {
		self.check_in_design()?;
	
		let design_rc = self.design.upgrade().unwrap();
		let mut design = design_rc.borrow_mut();
		let child = design.get_scope_mut(child_ref).unwrap(); // TODO fix unwrap
		child.set_parent(self.id)?;
		Ok(())
	}

	pub fn add_conditional_scope(&mut self, condition: Expression, child: ScopeRef) -> Result<ScopeRef, DesignError> {
		self.add_subscope(child)?;
		self.conditionals.push(ConditionalScope{condition, scope: child});
		Ok(child)

		// TODO assert condition valid in this scope
		// TODO assert condition is compile-time constant
	}

	pub fn add_loop_scope(&mut self, iterator_var: SignalRef, iterator_begin: Expression, iterator_end: Expression, child: ScopeRef) -> Result<ScopeRef, DesignError> {
		self.add_subscope(child)?;
		self.loops.push(RangeScope{iterator_var, iterator_begin, iterator_end, scope: child});
		Ok(child)

		// TODO assert iterator var in child scope
		// TODO assert expressions valid in this scope
		// TODO assert iterator_var type is int
		// TODO assert iterator_begin is compile-time constant
		// TODO assert iterator_end is compile-time constant
	}

	pub fn assign_signal(&mut self, signal: SignalRef, expr: Expression) -> Result<(), DesignError> {
		self.assignments.push(Assignment{lhs: signal, rhs: expr});
		Ok(())

		// TODO assert signal accessible from this scope
		// TODO expression valid in this scope
	}

	pub fn new_signal(&mut self) -> Result<SignalBuilder, DesignError> {
		self.check_in_design()?;
		let design_rc = self.design.upgrade().unwrap();
		let mut design = design_rc.borrow_mut();
		Ok(design.new_signal(self.id))
	}

}