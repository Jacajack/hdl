use super::DesignError;
use super::DesignHandle;
use super::Expression;
use super::ScopeRef;
use super::SignalRef;
use super::WeakDesignHandle;

pub enum SignalClass {
	Logic,
	Signed,
	Unsigned,
}

pub struct EdgeSensitivity {
	pub clock_signal: SignalRef,
	pub on_rising: bool,
}

pub struct SensitivityList {
	pub edges: Vec<EdgeSensitivity>,
}

pub enum SignalSensitivity {
	Async,
	Comb(SensitivityList),
	Sync(SensitivityList),
	Clock,
	Const,
}

pub enum ArraySlice {
	Range(Box<Expression>, Box<Expression>),
	Index(Box<Expression>),
}

pub struct SignalType {
	pub width: Box<Expression>, 
	pub class: SignalClass,
	pub senitivity: SignalSensitivity,
}

pub struct SignalSlice {
	pub signal: SignalRef,
	pub slices: Vec<ArraySlice>,
	// TODO bit slice?
}

pub struct Signal {
	pub(super) design: WeakDesignHandle,
	pub(super) id: SignalRef,
	pub parent_scope: ScopeRef,
	pub name: String,
	pub dimensions: Vec<Expression>,
	pub bit_width: Expression,
	pub class: SignalClass,
	pub sensitivity: SignalSensitivity,
}

pub enum SignalDirection {
	Input,
	Output,
	Tristate,
}

pub struct InterfaceSignal {
	pub signal: Signal,
	pub direction: SignalDirection,
}

// TODO index op on SignalRef
// TODO index op on SignalSlice

impl Signal {
	pub(super) fn set_design(&mut self, design: WeakDesignHandle, id: usize) {
		assert!(self.id.is_null());
		self.design = design;
		self.id = SignalRef{id};
	}
}

pub struct SignalBuilder {
	design: DesignHandle,
	scope: ScopeRef,
	name: Option<String>,
	dimensions: Vec<Expression>,
	bit_width: Option<Expression>,
	class: Option<SignalClass>,
	sensitivity: Option<SignalSensitivity>,
}

impl SignalBuilder {
	pub fn new(design: DesignHandle, scope: ScopeRef) -> Self {
		Self {
			design,
			scope,
			name: None,
			dimensions: vec![],
			bit_width: None,
			class: None,
			sensitivity: None,
		}
	}

	pub fn name(mut self, name: &str) -> Self {
		// TODO check name constraints
		self.name = Some(name.to_string());
		self
	}

	pub fn width(mut self, width: Expression) -> Self {
		// TODO assert bit width constant
		self.bit_width = Some(width);
		self
	}

	pub fn class(mut self, class: SignalClass) -> Self {
		self.class = Some(class);
		self
	}

	pub fn constant(self) -> Self {
		self.sensitivity(SignalSensitivity::Const)
	}

	pub fn asynchronous(self) -> Self {
		self.sensitivity(SignalSensitivity::Async)
	}

	pub fn clock(self) -> Self {
		self.sensitivity(SignalSensitivity::Clock)
	}

	pub fn sensitivity(mut self, sensitivity: SignalSensitivity) -> Self {
		self.sensitivity = Some(sensitivity);
		self
	}

	pub fn build(self) -> Result<SignalRef, DesignError> {
		// TODO assert dimensions constant
		// TODO assert clocking lists valid

		Ok(self.design.borrow_mut().add_signal(Signal{
			design: WeakDesignHandle::new(),
			id: SignalRef{id: 0},
			parent_scope: self.scope,
			name: self.name.ok_or(DesignError::InvalidName)?,
			dimensions: self.dimensions,
			bit_width: self.bit_width.ok_or(DesignError::SignalWidthNotSpecified)?,
			class: self.class.ok_or(DesignError::SignalClassNotSpecified)?,
			sensitivity: self.sensitivity.ok_or(DesignError::SignalSensitivityNotSpecified)?,
		}))
	}
}