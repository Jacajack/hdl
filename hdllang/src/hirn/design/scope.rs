use std::collections::HashMap;
use super::signal::{SignalRef, SignalType, Signal, SignalDirection};
use super::functional_blocks::BlockInstance;
use super::Expression;
use super::expression::NumericConstant;

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

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ScopeRef {
	id: usize,
}

pub struct Scope {
	parent: Option<ScopeRef>,
	assignments: Vec<Assignment>,
	loops: Vec<RangeScope>,
	conditionals: Vec<ConditionalScope>,
	blocks: Vec<BlockInstance>,
}

impl Scope {
	pub fn new(parent: Option<ScopeRef>) -> Self {
		Self {
			parent,
			assignments: vec![],
			loops: vec![],
			conditionals: vec![],
			blocks: vec![],
		}
	}

	// pub fn add_signal(&mut self, design: &mut Design, signal: Signal) -> SignalRef {
	// 	let id = design.new_signal_id();
	// 	self.signals.insert(id, signal);
		
	// 	// FIXME
	// 	todo!();
	// }
}