use super::DesignError;
use super::DesignHandle;
use super::Expression;
use super::ScopeId;
use super::SignalId;

/// Determines representation of a signal
#[derive(Clone)]
pub enum SignalClass {
	/// Logic signal with given bit width (cannot be used in arithmetic)
	Logic(Box<Expression>), 

	/// Signed integer with given bit width
	Signed(Box<Expression>), 

	/// Unsigned integer with given bit width
	Unsigned(Box<Expression>), 

	/// Type used for compile-time generics. No bit width specified
	Generic, 
}

/// Determines sensitivity of a signal to certain clock edges
#[derive(Clone, Copy)]
pub struct EdgeSensitivity {
	pub clock_signal: SignalId,
	pub on_rising: bool,
}

/// Determines sensitivity of a signal to certain clocks
pub type ClockSensitivityList = Vec<EdgeSensitivity>;

/// Determines 'sensitivity' of a signal - i.e. how constant it is
#[derive(Clone)]
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

/// Specifies a slice of a signal array
#[derive(Clone)]
pub enum ArraySlice {
	/// Range of indices (both ends inclusive)
	Range(Box<Expression>, Box<Expression>),

	/// Single array element
	Index(Box<Expression>),
}

/// Determines which part of a signal (or signal array) is accessed
#[derive(Clone)]
pub struct SignalSlice {
	/// Signal being accessed
	pub signal: SignalId,

	/// Array of slices, one per signal dimension and lastly for signal width
	pub slices: Vec<ArraySlice>,
}

impl From<SignalId> for SignalSlice {
	fn from(signal: SignalId) -> Self {
		Self {
			signal,
			slices: vec![],
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

/// Specifies direction for signals in module interface
pub enum SignalDirection {
	/// Input signal (from the perspective of the module)
	Input,

	/// Output signal (from the perspective of the module)
	Output,

	/// Tristate signal (direction left unspecified, actually)
	Tristate,
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
		// TODO check name constraints
		self.name = Some(name.to_string());
		self
	}

	/// Sets type to unsigned and specifies width
	pub fn unsigned(mut self, width: Expression) -> Self {
		// TODO assert bit width constant
		assert!(self.class.is_none());
		self.class = Some(SignalClass::Unsigned(Box::new(width)));
		self
	}

	/// Sets type to signed and specifies width
	pub fn signed(mut self, width: Expression) -> Self {
		// TODO assert bit width constant
		assert!(self.class.is_none());
		self.class = Some(SignalClass::Signed(Box::new(width)));
		self
	}

	/// Sets type to logic and specifies width
	pub fn logic(mut self, width: Expression) -> Self {
		// TODO assert bit width constant
		assert!(self.class.is_none());
		self.class = Some(SignalClass::Logic(Box::new(width)));
		self
	}

	/// Sets type to generic
	pub fn generic(mut self) -> Self {
		assert!(self.class.is_none());
		self.class = Some(SignalClass::Generic);
		self
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
		let edge = EdgeSensitivity{
			clock_signal: clock,
			on_rising,
		};

		if let Some(list) = &mut self.comb_clocking {
			list.push(edge);
		} else {
			self.comb_clocking = Some(vec![edge]);
		}

		self
	}

	/// Marks signal as synchronous and sensitive to the specified clock.
	/// Calling multiple times will add additional clocks to the sensitivity list.
	pub fn sync(mut self, clock: SignalId, on_rising: bool) -> Self {
		assert!(self.sensitivity.is_none());
		assert!(self.comb_clocking.is_none());
		let edge = EdgeSensitivity{
			clock_signal: clock,
			on_rising,
		};

		if let Some(list) = &mut self.sync_clocking {
			list.push(edge);
		} else {
			self.sync_clocking = Some(vec![edge]);
		}
		
		self
	}

	/// Creates the signal and adds it to the design. Returns the signal ID.
	pub fn build(self) -> Result<SignalId, DesignError> {
		// TODO assert dimensions constant
		// TODO assert clocking lists valid

		let sensitivity = match (self.sensitivity, self.comb_clocking, self.sync_clocking) {
			(Some(s), None, None) => s,
			(None, Some(c), None) => SignalSensitivity::Comb(c),
			(None, None, Some(s)) => SignalSensitivity::Sync(s),
			(None, None, None) => return Err(DesignError::SignalSensitivityNotSpecified),
			_ => return Err(DesignError::ConflictingSignalSensitivity),
		};

		Ok(self.design.borrow_mut().add_signal(Signal{
			id: SignalId{id: 0},
			parent_scope: self.scope,
			name: self.name.ok_or(DesignError::InvalidName)?,
			dimensions: self.dimensions,
			class: self.class.ok_or(DesignError::SignalClassNotSpecified)?,
			sensitivity: sensitivity,
		}))
	}
}