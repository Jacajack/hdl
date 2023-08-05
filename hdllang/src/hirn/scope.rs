use std::collections::HashMap;
use super::design::{Design, ScopeId, SignalId};
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

pub struct ScopeRef {
	id: ScopeId,
}

pub struct Scope {
	parent: Option<ScopeRef>,
	signals: HashMap<SignalId, Signal>,
	assignments: Vec<Assignment>,
	loops: Vec<RangeScope>,
	conditionals: Vec<ConditionalScope>,
	blocks: Vec<BlockInstance>,
}

impl Scope {
	pub fn new(parent: Option<ScopeRef>) -> Self {
		Self {
			parent,
			signals: HashMap::new(),
			assignments: vec![],
			loops: vec![],
			conditionals: vec![],
			blocks: vec![],
		}
	}

	pub fn add_signal(&mut self, design: &mut Design, signal: Signal) -> SignalRef {
		let id = design.new_signal_id();
		self.signals.insert(id, signal);
		
		// FIXME
		SignalRef {
			signal_id: id,
			slices: vec![],
			bit_range: (
				Box::new(Expression::Constant(NumericConstant::zero())),
				Box::new(Expression::Constant(NumericConstant::zero()))),
		}
	}
}