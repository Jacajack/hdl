use std::{fmt::Display, sync::Arc};

use log::debug;
use thiserror::Error;

use crate::design::{ModuleId, SignalId};

use super::{ElabAssumptionsBase, ElabSignal, GeneratedSignalRef, SignalMask};

pub trait SeverityPolicy {
	fn severity(&self, kind: &ElabMessageKind) -> ElabMessageSeverity;
}

pub struct DefaultSeverityPolicy;

impl SeverityPolicy for DefaultSeverityPolicy {
	fn severity(&self, kind: &ElabMessageKind) -> ElabMessageSeverity {
		use ElabMessageKind::*;
		use ElabMessageSeverity::*;
		match kind {
			SignalUnused { .. } => Warning,
			SignalNotDriven { .. } => Warning,
			SignalNotDrivenAndUsed { .. } => Error,
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
		for msg in other.messages.iter() {
			self.add_message(msg.clone());
		}
	}

	pub fn add_message(&mut self, msg: ElabMessage) {
		debug!("Elab message: {}", msg);
		self.messages.push(msg);
	}

	pub fn messages(&self) -> &[ElabMessage] {
		&self.messages
	}
}

#[derive(Clone, Debug)]
pub struct ElabMessage {
	kind: ElabMessageKind,
	module: ModuleId,
	assumptions: Arc<dyn ElabAssumptionsBase>,
}

impl ElabMessage {
	pub fn new(kind: ElabMessageKind, module: ModuleId, assumptions: Arc<dyn ElabAssumptionsBase>) -> Self {
		Self {
			kind,
			module,
			assumptions,
		}
	}

	pub fn kind(&self) -> &ElabMessageKind {
		&self.kind
	}

	pub fn default_severity(&self) -> ElabMessageSeverity {
		DefaultSeverityPolicy.severity(&self.kind)
	}

	pub fn severity(&self, policy: &dyn SeverityPolicy) -> ElabMessageSeverity {
		policy.severity(&self.kind)
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
		write!(f, "{:?}: {}", self.default_severity(), self.kind())
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
	#[error("Expression evaluation failed")]
	EvalError(#[from] crate::design::EvalError),

	#[error("Signal has no driver")]
	SignalNotDriven {
		signal: Box<GeneratedSignalRef>,
		elab: Box<ElabSignal>,
	},

	#[error("Signal is not being used")]
	SignalUnused {
		signal: Box<GeneratedSignalRef>,
		elab: Box<ElabSignal>,
	},

	#[error("Signal has more that one driver")]
	SignalConflict {
		signal: Box<GeneratedSignalRef>,
		elab: Box<ElabSignal>,
	},

	#[error("Signal is being used despite not having a driver")]
	SignalNotDrivenAndUsed {
		signal: Box<GeneratedSignalRef>,
		elab: Box<ElabSignal>,
	},

	#[error("The design contains a combinational loop")]
	CombLoop,

	#[error("Assignment/binding of signals with different widths")]
	WidthMismatch {
		lhs: Box<GeneratedSignalRef>,
		lhs_width: u64,
		rhs_width: u64,
	},

	#[error("Partial array assignment is not supported")]
	PartialArrayAssignment {
		lhs: Box<GeneratedSignalRef>,
		rhs: Box<GeneratedSignalRef>,
	},

	#[error("Array dimensions mismatch in assignment")]
	ArraySizeMismatch {
		lhs: Box<GeneratedSignalRef>,
		rhs: Box<GeneratedSignalRef>,
		lhs_dimensions: Vec<usize>,
		rhs_dimensions: Vec<usize>,
	},

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

	#[error("Invalid array index")]
	InvalidArrayIndex, // TODO SignalId
}
