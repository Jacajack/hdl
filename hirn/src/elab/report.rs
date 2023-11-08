use std::{fmt::Display, sync::Arc};

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
			SignalUnused(_) => Warning,
			SignalPartiallyUnused => Warning,
			SignalNotDriven(_) => Error,
			SignalPartiallyDriven => Error,
			CombLoop => Error,
			WidthMismatch => Error,
			Notice(_) => Info,
			MaxForIterCount => Error,
			CyclicGenericDependency => Error,
			MultipleGenericAssignments => Error,
			UnassignedGeneric => Error,
			NotDrivable => Error,
			EvalError(_) => Error,
			_ => Error, // FIXME
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
	assumptions: Arc<dyn ElabAssumptionsBase>,
}

impl ElabMessage {
	pub fn new(
		kind: ElabMessageKind,
		module: ModuleId,
		assumptions: Arc<dyn ElabAssumptionsBase>,
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

	#[error("Expression evaluation failed")]
	EvalError(#[from] crate::design::EvalError),

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

	#[error("Too many for iterations")]
	MaxForIterCount, // FIXME iterator signal ID

	#[error("Cyclic generic variable dependency")]
	CyclicGenericDependency, // FIXME signal IDs

	#[error("Generic variable assigned more than once")]
	MultipleGenericAssignments, // FIXME signal IDs

	#[error("Unassigned generic variable")]
	UnassignedGeneric, // FIXME signal ID

	#[error("Target signal is not drivable")]
	NotDrivable, // FIXME signal ID

	#[error("Invalid signal width")]
	InvalidSignalWidth(i64), // TODO SignalId

	#[error("Invalid array dimension")]
	InvalidArrayDimension(i64), // TODO SignalId,

	#[error("Invalid array rank")]
	InvalidArrayRank(u32), // TODO SignalId

	#[error("Invalid array size")]
	InvalidArraySize(usize), // TODO SignalID

	#[error("Invalid signal bit range")]
	InvalidSignalBitRange, // TODO SignalId
}
