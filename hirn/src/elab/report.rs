use thiserror::Error;

use crate::design::{SignalId, ModuleId};

use super::ElabAssumptionsBase;

#[derive(Clone, Debug, Default)]
pub struct ElabReport {
	messages: Vec<ElabMessage>,
}

impl ElabReport {
	pub fn extend(&mut self, other: ElabReport) {
		self.messages.extend(other.messages);
	}

	pub fn messages(&self) -> &[ElabMessage] {
		&self.messages
	}
}

#[derive(Clone, Debug)]
pub struct ElabMessage {
	severity: ElabMessageSeverity,
	kind: ElabMessageKind,
	module: ModuleId,
	assumptions: Box<dyn ElabAssumptionsBase>,
}

impl ElabMessage {
	pub fn kind(&self) -> &ElabMessageKind {
		&self.kind
	}

	pub fn severity(&self) -> ElabMessageSeverity {
		self.severity
	}

	pub fn module_id(&self) -> ModuleId {
		self.module
	}

	pub fn assumptions(&self) -> &dyn ElabAssumptionsBase {
		&*self.assumptions
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ElabMessageSeverity {
	Info, Warning, Error
}

#[derive(Clone, Debug, Error)]
pub enum ElabMessageKind {
	#[error("Signal has no driver")]
	SignalNotDriven(SignalId),

	#[error("Some bits in signal are not driven")]
	SignalPartiallyDriven,

	#[error("Signal is not being used")]
	SignalUnused(SignalId),

	#[error("Some bits in signal are not being used")]
	SignalPartiallyUnused,

	#[error("The design contains a combinational loop")]
	CombLoop,

	#[error("Assignment/binding of signals with different widths")]
	WidthMismatch,
}

