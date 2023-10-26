use std::fmt::Display;

use thiserror::Error;

use crate::design::{ModuleId, SignalId};

use super::ElabAssumptionsBase;

pub trait SeverityPolicy {
	fn severity(&self, kind: &ElabMessageKind) -> ElabMessageSeverity;
}

pub struct DefaultSeverityPolicy;

impl SeverityPolicy for DefaultSeverityPolicy {
	fn severity(&self, kind: &ElabMessageKind) -> ElabMessageSeverity {
		use ElabMessageKind::*;
		use ElabMessageSeverity::*;
		match kind {
			SignalUnused(_) => ElabMessageSeverity::Warning,
			SignalPartiallyUnused => ElabMessageSeverity::Warning,
			SignalNotDriven(_) => Error,
			SignalPartiallyDriven => Error,
			CombLoop => Error,
			WidthMismatch => Error,
			Notice(_) => Info,
		}
	}
}

#[derive(Clone, Debug, Default)]
pub struct ElabReport {
	messages: Vec<ElabMessage>,
}

impl ElabReport {
	pub fn extend(&mut self, other: &ElabReport) {
		self.messages.extend(other.messages.clone());
	}

	pub fn add_message(&mut self, msg: ElabMessage) {
		self.messages.push(msg);
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
	pub fn new(
		kind: ElabMessageKind,
		module: ModuleId,
		assumptions: Box<dyn ElabAssumptionsBase>,
		policy: Option<&dyn SeverityPolicy>,
	) -> Self {
		let severity = 
			policy.unwrap_or(&DefaultSeverityPolicy)
			.severity(&kind);

		Self {
			severity,
			kind,
			module,
			assumptions,
		}
	}

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

impl Display for ElabMessage {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}: {}", self.severity(), self.kind())
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ElabMessageSeverity {
	Info,
	Warning,
	Error,
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

	#[error("Notice for the user")]
	Notice(String),
}
