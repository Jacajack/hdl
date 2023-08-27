use super::DesignError;
use super::DesignHandle;
use super::Expression;
use super::ScopeId;
use super::SignalId;
use std::collections::HashSet;

/// Potential TODO: Logic type which cannot be used in arithmetic
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SignalSignedness {
	/// Signed integer with given bit width
	Signed,

	/// Unsigned integer with given bit width
	Unsigned,
}

/// Determines representation of a signal
#[derive(Clone)]
pub struct SignalClass {
	pub signedness: SignalSignedness, // TODO make priv
	pub width: Box<Expression>,
}

impl SignalClass {
	pub fn new(expr: Expression, signedness: SignalSignedness) -> SignalClass {
		Self {
			signedness,
			width: Box::new(expr),
		}
	}

	pub fn new_signed(expr: Expression) -> SignalClass {
		Self::new(expr, SignalSignedness::Signed)
	}

	pub fn new_unsigned(expr: Expression) -> SignalClass {
		Self::new(expr, SignalSignedness::Unsigned)
	}
}

/// Determines sensitivity of a signal to certain clock edges
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct EdgeSensitivity {
	pub clock_signal: SignalId,
	pub on_rising: bool,
}

/// Determines sensitivity of a signal to certain clocks
#[derive(Clone, Default, Debug)]
pub struct ClockSensitivityList(HashSet<EdgeSensitivity>);

impl ClockSensitivityList {
	pub fn new(edge: &EdgeSensitivity) -> Self {
		let mut list = Self { 0: HashSet::new() };
		list.push(*edge);
		list
	}

	pub fn push(&mut self, edge: EdgeSensitivity) {
		self.0.insert(edge);
	}

	pub fn combine(&self, other: &ClockSensitivityList) -> Self {
		let mut new = self.clone();
		new.0.extend(other.0.iter());
		new
	}

	pub fn is_subset_of(&self, other: &ClockSensitivityList) -> bool {
		self.0.is_subset(&other.0)
	}
}

/// Determines 'sensitivity' of a signal - i.e. how constant it is
#[derive(Clone, Debug)]
pub enum SignalSensitivity {
	/// Completely asynchronous signal. Latching with any clock can lead to metastability
	Async,

	/// Combinational signal derived from syncrhonous signals clocked by the specified clocks
	Comb(ClockSensitivityList),

	/// Synchronous signal clocked by the specified clocks
	Sync(ClockSensitivityList),

	/// A clock signal
	Clock,

	/// A compile-time constant signal
	Const,
}

impl SignalSensitivity {
	/// Determines sensitivity of a signal resulting form combining two signals
	/// with combinational logic
	pub fn combine(&self, other: &SignalSensitivity) -> Option<Self> {
		use SignalSensitivity::*;
		Some(match (self, other) {
			(Const, Const) => Const,
			(Async, _) | (_, Async) => Async,
			(Clock, _) | (_, Clock) => None?,
			(Sync(lhs), Sync(rhs)) => Sync(lhs.combine(rhs)),
			(Sync(lhs), Comb(rhs)) => Comb(lhs.combine(rhs)),
			(Comb(lhs), Sync(rhs)) => Comb(lhs.combine(rhs)),
			(Comb(lhs), Comb(rhs)) => Comb(lhs.combine(rhs)),
			(Sync(lhs), _) | (_, Sync(lhs)) => Sync(lhs.clone()),
			(Comb(lhs), _) | (_, Comb(lhs)) => Comb(lhs.clone()),
		})
	}

	/// Determines whether this signal can drive the other specified signal
	/// The logic in this function implements both sensitivity and clocking semantics
	pub fn can_drive(&self, dest: &SignalSensitivity) -> bool {
		use SignalSensitivity::*;
		match (dest, self) {
			(Const, Const) => true,
			(Clock, Clock) => true,
			(Sync(_), Const) => true,
			(Sync(lhs), Sync(rhs)) => rhs.is_subset_of(lhs),
			(Comb(_), Const) => true,
			(Comb(lhs), Comb(rhs)) => rhs.is_subset_of(lhs),
			(Comb(lhs), Sync(rhs)) => rhs.is_subset_of(lhs),
			(Async, Async | Const | Comb(_) | Sync(_) | Clock) => true,
			_ => false,
		}
	}
}

/// Determines which part of a signal (or signal array) is accessed
#[derive(Clone)]
pub struct SignalSlice {
	/// Signal being accessed
	pub signal: SignalId,

	/// Array of indices, one per dimension
	pub indices: Vec<Expression>,
}

impl From<SignalId> for SignalSlice {
	fn from(signal: SignalId) -> Self {
		Self {
			signal,
			indices: vec![],
		}
	}
}

/// Physical signal representation
pub struct Signal {
	/// Self-reference
	pub(super) id: SignalId,

	/// Parent scope ID
	pub parent_scope: ScopeId,

	/// Name of the signal
	pub name: String,

	/// Dimensions of the signal (for array signals, empty otherwise)
	pub dimensions: Vec<Expression>,

	/// Signal representation
	pub class: SignalClass,

	/// Variability level
	pub sensitivity: SignalSensitivity,
}

impl Signal {
	fn new(
		id: SignalId,
		scope: ScopeId,
		name: &str,
		dimensions: Vec<Expression>,
		class: SignalClass,
		sensitivity: SignalSensitivity,
	) -> Result<Self, DesignError> {
		// Check name valid
		if !super::utils::is_name_valid(name) {
			return Err(DesignError::InvalidName);
		}

		// TODO assert dimensions constant
		// TODO assert clocking lists valid
		// TODO assert class width valid if applicable

		Ok(Self {
			id,
			parent_scope: scope,
			name: name.into(),
			dimensions,
			class,
			sensitivity,
		})
	}

	pub fn is_scalar(&self) -> bool {
		self.dimensions.is_empty()
	}
}

/// Signal builder helper
pub struct SignalBuilder {
	/// Handle to the design where the signal will be addded
	design: DesignHandle,

	/// Scope where the signal will be located
	scope: ScopeId,

	/// Name for the signal
	name: Option<String>,

	/// Dimensions (arrays only)
	dimensions: Vec<Expression>,

	/// Signal representation and width
	class: Option<SignalClass>,

	/// Sensitivity level
	sensitivity: Option<SignalSensitivity>,

	/// Clocking list (for combinational signals)
	comb_clocking: Option<ClockSensitivityList>,

	/// Clocking list (for synchronous signals)
	sync_clocking: Option<ClockSensitivityList>,
}

impl SignalBuilder {
	/// Starts building a new signal
	pub fn new(design: DesignHandle, scope: ScopeId) -> Self {
		Self {
			design,
			scope,
			name: None,
			dimensions: vec![],
			class: None,
			sensitivity: None,
			comb_clocking: None,
			sync_clocking: None,
		}
	}

	/// Sets name of the signal (required)
	pub fn name(mut self, name: &str) -> Self {
		self.name = Some(name.to_string());
		self
	}

	/// Sets type to unsigned and specifies width
	pub fn unsigned(mut self, width: Expression) -> Self {
		assert!(self.class.is_none());
		self.class = Some(SignalClass::new_unsigned(width));
		self
	}

	/// Sets type to signed and specifies width
	pub fn signed(mut self, width: Expression) -> Self {
		assert!(self.class.is_none());
		self.class = Some(SignalClass::new_signed(width));
		self
	}

	pub fn wire(self) -> Self {
		self.unsigned(Expression::new_one())
	}

	/// Marks signal as constant
	pub fn constant(self) -> Self {
		self.sensitivity(SignalSensitivity::Const)
	}

	/// Marks signal as asynchronous
	pub fn asynchronous(self) -> Self {
		self.sensitivity(SignalSensitivity::Async)
	}

	/// Marks signal as clock signal
	pub fn clock(self) -> Self {
		self.sensitivity(SignalSensitivity::Clock)
	}

	/// Sets signal sensitivity
	pub fn sensitivity(mut self, sensitivity: SignalSensitivity) -> Self {
		assert!(self.sensitivity.is_none());
		assert!(self.comb_clocking.is_none());
		assert!(self.sync_clocking.is_none());
		self.sensitivity = Some(sensitivity);
		self
	}

	/// Marks signal and combinational and sensitive to the specified clock.
	/// Calling multiple times will add additional clocks to the sensitivity list.
	pub fn comb(mut self, clock: SignalId, on_rising: bool) -> Self {
		assert!(self.sensitivity.is_none());
		assert!(self.sync_clocking.is_none());
		let edge = EdgeSensitivity {
			clock_signal: clock,
			on_rising,
		};

		if let Some(list) = &mut self.comb_clocking {
			list.push(edge);
		}
		else {
			self.sync_clocking = Some(ClockSensitivityList::new(&edge));
		}

		self
	}

	/// Marks signal as synchronous and sensitive to the specified clock.
	/// Calling multiple times will add additional clocks to the sensitivity list.
	pub fn sync(mut self, clock: SignalId, on_rising: bool) -> Self {
		assert!(self.sensitivity.is_none());
		assert!(self.comb_clocking.is_none());
		let edge = EdgeSensitivity {
			clock_signal: clock,
			on_rising,
		};

		if let Some(list) = &mut self.sync_clocking {
			list.push(edge);
		}
		else {
			self.sync_clocking = Some(ClockSensitivityList::new(&edge));
		}

		self
	}

	/// Adds a dimension to the signal array
	pub fn array(mut self, expr: Expression) -> Result<Self, DesignError> {
		// TODO assert dimensions valid if can be evaluated

		self.dimensions.push(expr);
		Ok(self)
	}

	/// Creates the signal and adds it to the design. Returns the signal ID.
	pub fn build(self) -> Result<SignalId, DesignError> {
		let sensitivity = match (self.sensitivity, self.comb_clocking, self.sync_clocking) {
			(Some(s), None, None) => s,
			(None, Some(c), None) => SignalSensitivity::Comb(c),
			(None, None, Some(s)) => SignalSensitivity::Sync(s),
			(None, None, None) => return Err(DesignError::SignalSensitivityNotSpecified),
			_ => return Err(DesignError::ConflictingSignalSensitivity),
		};

		self.design.borrow_mut().add_signal(Signal::new(
			SignalId { id: 0 },
			self.scope,
			self.name.ok_or(DesignError::InvalidName)?.as_str(),
			self.dimensions,
			self.class.ok_or(DesignError::SignalClassNotSpecified)?,
			sensitivity,
		)?)
	}
}
