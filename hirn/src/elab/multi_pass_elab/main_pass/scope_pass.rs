use crate::design::ScopeId;

use super::MainPassCtx;

/// IDs generated each time a scope is visited
#[derive(Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct ScopePassId(usize);

/// Auxiliary information about a scope pass
#[derive(Clone, Copy, Debug)]
pub enum ScopePassInfo {
	Unconditional {
		id: ScopeId,
		parent_id: Option<ScopeId>,
	},
	Conditional {
		id: ScopeId,
		parent_id: ScopeId,
		was_true: bool,
	},
	Range {
		id: ScopeId,
		parent_id: ScopeId,
		iter_number: i64,
	},
}

impl MainPassCtx {
	/// Records that an unconditional scope has been visited
	pub(super) fn record_scope_pass(&mut self, id: ScopeId) -> ScopePassId {
		let handle = self.design.get_scope_handle(id).expect("scope not in design");
		let pass_id = ScopePassId(self.scope_pass_counter);
		self.scope_pass_counter += 1;
		self.pass_info.insert(
			pass_id,
			ScopePassInfo::Unconditional {
				id,
				parent_id: handle.parent(),
			},
		);
		self.current_pass.insert(id, pass_id);
		pass_id
	}

	/// Records that a conditional scope has been visited
	pub(super) fn record_conditional_scope_pass(&mut self, id: ScopeId, cond_true: bool) -> ScopePassId {
		let handle = self.design.get_scope_handle(id).expect("scope not in design");
		let pass_id = ScopePassId(self.scope_pass_counter);
		self.scope_pass_counter += 1;
		self.pass_info.insert(
			pass_id,
			ScopePassInfo::Conditional {
				id,
				parent_id: handle.parent().expect("conditional scope has no parent"),
				was_true: cond_true,
			},
		);
		self.current_pass.insert(id, pass_id);
		pass_id
	}

	/// Recrods a pass through a range scope
	pub(super) fn record_range_scope_pass(&mut self, id: ScopeId, iter: i64) -> ScopePassId {
		let handle = self.design.get_scope_handle(id).expect("scope not in design");
		let pass_id = ScopePassId(self.scope_pass_counter);
		self.scope_pass_counter += 1;
		self.pass_info.insert(
			pass_id,
			ScopePassInfo::Range {
				id,
				parent_id: handle.parent().expect("range scope has no parent"),
				iter_number: iter,
			},
		);
		self.current_pass.insert(id, pass_id);
		pass_id
	}

	/// Returns current pass ID for a scope
	pub fn get_scope_pass_id(&self, id: ScopeId) -> ScopePassId {
		self.current_pass.get(&id).copied().expect("scope pass not recorded!")
	}
}
