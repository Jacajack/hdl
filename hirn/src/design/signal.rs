use super::Evaluates;
use super::DesignError;
use super::DesignHandle;
use super::EvalContext;
use super::EvaluatesType;
use super::Expression;
use super::HasComment;
use super::ScopeId;
use super::SignalId;
use std::collections::HashMap;
use std::collections::HashSet;

/// Potential TODO: Logic type which cannot be used in arithmetic
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SignalSignedness {
	/// Signed integer with given bit width
	Signed,

	/// Unsigned integer with given bit width
	Unsigned,
}

impl SignalSignedness {
	pub fn is_signed(&self) -> bool {
		matches!(self, SignalSignedness::Signed)
	}

	pub fn is_unsigned(&self) -> bool {
		matches!(self, SignalSignedness::Unsigned)
	}
}

pub trait HasSignedness {
	fn signedness(&self) -> SignalSignedness;

	fn is_signed(&self) -> bool {
		self.signedness() == SignalSignedness::Signed
	}

	fn is_unsigned(&self) -> bool {
		self.signedness() == SignalSignedness::Unsigned
	}
}

/// Determines representation of a signal
#[derive(Clone, Debug)]
pub struct SignalClass {
	signedness: SignalSignedness,
	width: Box<Expression>,
	is_wire: bool,
}

impl SignalClass {
	pub fn new(width: Expression, signedness: SignalSignedness) -> SignalClass {
		Self {
			signedness,
			width: Box::new(width),
			is_wire: false,
		}
	}

	pub fn new_signed(width: Expression) -> SignalClass {
		Self::new(width, SignalSignedness::Signed)
	}

	pub fn new_unsigned(width: Expression) -> SignalClass {
		Self::new(width, SignalSignedness::Unsigned)
	}

	pub fn new_wire() -> SignalClass {
		Self {
			signedness: SignalSignedness::Unsigned,
			width: Box::new(Expression::new_one()),
			is_wire: true,
		}
	}

	pub fn width(&self) -> &Expression {
		&self.width
	}

	pub fn signedness(&self) -> SignalSignedness {
		self.signedness
	}

	pub fn is_wire(&self) -> bool {
		self.is_wire
	}
}

impl HasSignedness for SignalClass {
	fn signedness(&self) -> SignalSignedness {
		self.signedness
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
	pub fn new_empty() -> Self {
		let list = Self { 0: HashSet::new() };
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

	pub fn substitute_clocks(&mut self, clock_map: &HashMap<SignalId, SignalId>) {
		self.0 = self
			.0
			.iter()
			.map(|e| EdgeSensitivity {
				clock_signal: *clock_map.get(&e.clock_signal).unwrap_or(&e.clock_signal),
				on_rising: e.on_rising,
			})
			.collect();
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

	/// An invariable signal. When used in interface, does not become a generic parameter.
	Const,

	/// Compile-time known signal. When used in interface, becomes a generic parameter.
	Generic,
}

pub trait HasSensitivity {
	fn sensitivity(&self) -> &SignalSensitivity;

	fn is_async(&self) -> bool {
		matches!(self.sensitivity(), SignalSensitivity::Async)
	}

	fn is_comb(&self) -> bool {
		matches!(self.sensitivity(), SignalSensitivity::Comb(_))
	}

	fn is_sync(&self) -> bool {
		matches!(self.sensitivity(), SignalSensitivity::Sync(_))
	}

	fn is_clock(&self) -> bool {
		matches!(self.sensitivity(), SignalSensitivity::Clock)
	}

	fn is_const(&self) -> bool {
		matches!(self.sensitivity(), SignalSensitivity::Const)
	}

	fn is_generic(&self) -> bool {
		matches!(self.sensitivity(), SignalSensitivity::Generic)
	}
}

impl HasSensitivity for SignalSensitivity {
	fn sensitivity(&self) -> &SignalSensitivity {
		self
	}
}

impl SignalSensitivity {
	/// Determines sensitivity of a signal resulting form combining two signals
	/// with combinational logic
	pub fn combine(&self, other: &SignalSensitivity) -> Option<Self> {
		use SignalSensitivity::*;
		Some(match (self, other) {
			(Async, _) | (_, Async) => Async,
			(Sync(lhs), Sync(rhs)) => Comb(lhs.combine(rhs)),
			(Sync(lhs), Comb(rhs)) => Comb(lhs.combine(rhs)),
			(Comb(lhs), Sync(rhs)) => Comb(lhs.combine(rhs)),
			(Comb(lhs), Comb(rhs)) => Comb(lhs.combine(rhs)),
			(Comb(lhs), Const | Generic) | (Const | Generic, Comb(lhs)) => Comb(lhs.clone()),
			(Sync(lhs), Const | Generic) | (Const | Generic, Sync(lhs)) => Comb(lhs.clone()),
			(Clock, _) | (_, Clock) => None?,
			(Const, Const) | (Const, Generic) | (Generic, Const) => Const,
			(Generic, Generic) => Generic,
		})
	}

	/// Returns worse of two signal sensitivities
	pub fn or_worse(&self, other: &SignalSensitivity) -> Self {
		use SignalSensitivity::*;
		match (self, other) {
			(Async, _) | (_, Async) => Async,
			(Comb(lhs), Comb(rhs)) => Comb(lhs.combine(rhs)),
			(Sync(lhs), Sync(rhs)) => Sync(lhs.combine(rhs)),
			(Sync(lhs), Comb(rhs)) | (Comb(lhs), Sync(rhs)) => Comb(lhs.combine(rhs)),
			(Comb(sen), _) | (_, Comb(sen)) => Comb(sen.clone()),
			(Sync(sen), _) | (_, Sync(sen)) => Sync(sen.clone()),
			(Clock, _) | (_, Clock) => Clock,
			(Const, _) | (_, Const) => Const,
			(Generic, Generic) => Generic,
		}
	}

	/// Determines whether this signal can drive the other specified signal
	/// This function does not check if clocking lists are compatible.
	pub fn can_drive(&self, dest: &SignalSensitivity) -> bool {
		use SignalSensitivity::*;
		match (dest, self) {
			(Generic, Generic) => true,
			(Const, Generic) => true,
			(Const, Const) => true,
			(Clock, Clock) => true,
			(Sync(_), Const) => true,
			(Sync(_), Sync(_)) => true,
			(Comb(_), Const) => true,
			(Comb(_), Comb(_)) => true,
			(Comb(_), Sync(_)) => true,
			(Async, Async | Const | Comb(_) | Sync(_) | Clock) => true,
			_ => false,
		}
	}

	// Same as `can_drive()` but also verifies whether clocking lists are valid
	pub fn can_drive_check_clk(&self, dest: &SignalSensitivity) -> bool {
		use SignalSensitivity::*;
		match (dest, self) {
			(Sync(lhs), Sync(rhs)) => rhs.is_subset_of(lhs),
			(Comb(lhs), Comb(rhs)) => rhs.is_subset_of(lhs),
			(Comb(lhs), Sync(rhs)) => rhs.is_subset_of(lhs),
			(dest, this) => this.can_drive(dest),
		}
	}

	/// Substitutes clocks in the sensitivity list
	pub fn substitute_clocks(&mut self, clock_map: &HashMap<SignalId, SignalId>) {
		use SignalSensitivity::*;
		match self {
			Sync(list) | Comb(list) => list.substitute_clocks(clock_map),
			_ => {},
		}
	}
}

/// Determines which part of a signal (or signal array) is accessed
#[derive(Clone, Debug)]
pub struct SignalSlice {
	/// Signal being accessed
	pub signal: SignalId,

	/// Array of indices, one per dimension
	pub indices: Vec<Expression>,
}

impl SignalSlice {
	pub fn id(&self) -> SignalId {
		self.signal
	}

	pub fn indices(&self) -> &[Expression] {
		&self.indices
	}

	pub fn rank(&self) -> usize {
		self.indices.len()
	}
}

impl From<SignalId> for SignalSlice {
	fn from(signal: SignalId) -> Self {
		Self {
			signal,
			indices: vec![],
		}
	}
}

/// Signal slice but with two expressions for lsb and msb index
#[derive(Clone, Debug)]
pub struct SignalSliceRange {
	slice: SignalSlice,
	lsb: Option<Expression>,
	msb: Option<Expression>,
}

impl From<SignalId> for SignalSliceRange {
	fn from(signal: SignalId) -> Self {
		Self {
			slice: SignalSlice::from(signal),
			lsb: None,
			msb: None,
		}
	}
}

impl SignalSliceRange {
	pub fn new_full(slice: SignalSlice) -> Self {
		Self {
			slice,
			lsb: None,
			msb: None,
		}
	}

	pub fn new(slice: SignalSlice, lsb: Expression, msb: Expression) -> Self {
		Self {
			slice,
			lsb: Some(lsb),
			msb: Some(msb),
		}
	}

	pub fn slice(&self) -> &SignalSlice {
		&self.slice
	}

	pub fn lsb(&self) -> Option<&Expression> {
		self.lsb.as_ref()
	}

	pub fn msb(&self) -> Option<&Expression> {
		self.msb.as_ref()
	}

	pub fn lsb_msb(&self) -> Option<(&Expression, &Expression)> {
		match (self.lsb.as_ref(), self.msb.as_ref()) {
			(Some(lsb), Some(msb)) => Some((lsb, msb)),
			_ => None,
		}
	}

	pub fn signal(&self) -> SignalId {
		self.slice.signal
	}

	pub fn is_full(&self) -> bool {
		assert_eq!(self.lsb.is_none(), self.msb.is_none());
		self.lsb.is_none()
	}
}

/// Physical signal representation
#[derive(Clone, Debug)]
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

	/// Source code comment
	comment: Option<String>,
}

impl HasComment for Signal {
	fn get_comment(&self) -> Option<String> {
		self.comment.clone()
	}
}

impl Signal {
	fn new(
		id: SignalId,
		scope: ScopeId,
		name: &str,
		dimensions: Vec<Expression>,
		class: SignalClass,
		sensitivity: SignalSensitivity,
		comment: Option<String>,
		is_generated: bool,
	) -> Result<Self, DesignError> {
		// Check name valid
		if !super::utils::is_name_valid(name) {
			return Err(DesignError::InvalidName);
		}

		// Effective name
		let name = match is_generated {
			true => format!("{}_gen$", name),
			false => name.into(),
		};

		Ok(Self {
			id,
			parent_scope: scope,
			name,
			dimensions,
			class,
			sensitivity,
			comment,
		})
	}

	pub fn is_scalar(&self) -> bool {
		self.dimensions.is_empty()
	}

	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn rank(&self) -> usize {
		self.dimensions.len()
	}

	pub fn sensitivity(&self) -> &SignalSensitivity {
		&self.sensitivity
	}

	pub fn signedness(&self) -> SignalSignedness {
		self.class.signedness()
	}

	pub fn is_array(&self) -> bool {
		!self.is_scalar()
	}

	pub fn is_wire(&self) -> bool {
		self.class.is_wire()
	}

	pub fn width(&self) -> Expression {
		self.class.width().clone()
	}

	pub fn comment(&mut self, comment: &str) {
		self.comment = Some(comment.into());
	}
}

impl HasSignedness for Signal {
	fn signedness(&self) -> SignalSignedness {
		self.class.signedness()
	}
}

impl HasSensitivity for Signal {
	fn sensitivity(&self) -> &SignalSensitivity {
		&self.sensitivity
	}
}

/// Signal builder helper
pub struct SignalBuilder {
	/// Handle to the design where the signal will be addded
	design: DesignHandle,

	/// Scope where the signal will be located
	scope: ScopeId,

	/// Name for the signal
	name: String,

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

	/// Source code comment
	comment: Option<String>,

	/// Is signal generated (intermediate/temporary)
	is_generated: bool,
}

impl SignalBuilder {
	/// Starts building a new signal
	pub fn new(design: DesignHandle, scope: ScopeId, name: &str) -> Self {
		Self {
			design,
			scope,
			name: name.into(),
			dimensions: vec![],
			class: None,
			sensitivity: None,
			comb_clocking: None,
			sync_clocking: None,
			comment: None,
			is_generated: false,
		}
	}

	/// Creates a wire signal
	pub fn wire(mut self) -> Self {
		assert!(self.class.is_none());
		self.class = Some(SignalClass::new_wire());
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

	/// Marks signal as constant
	pub fn constant(self) -> Self {
		self.sensitivity(SignalSensitivity::Const)
	}

	/// Marks signal as generic constant
	pub fn generic(self) -> Self {
		self.sensitivity(SignalSensitivity::Generic)
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
			self.comb_clocking = Some(ClockSensitivityList::new(&edge));
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
		self.dimensions.push(expr);
		Ok(self)
	}

	/// Adds a source code comment
	pub fn comment(mut self, comment: &str) -> Self {
		self.comment = Some(comment.into());
		self
	}

	/// Marks signal as 'generated' so a special suffix can be
	/// added at codegen stage
	pub fn generated(mut self) -> Self {
		self.is_generated = true;
		self
	}

	fn validate(&self) -> Result<(), DesignError> {
		let scope;
		{
			// Note: this cannot be borrowed while we call validation and so on.
			let design_handle = self.design.borrow();
			scope = design_handle
				.get_scope_handle(self.scope)
				.expect("Scope must be in design");
		}

		// Validate width expression (generic)
		let eval_ctx = EvalContext::without_assumptions(self.design.clone());
		let class = self
			.class
			.as_ref()
			.expect("signal class must be specified before validation");
		let width_expr = class.width();
		width_expr.validate_no_assumptions(&scope)?;
		let width_type = width_expr.eval_type(&eval_ctx)?;

		if !width_type.sensitivity.is_generic() {
			return Err(DesignError::VariableSignalWidth);
		}

		// Check if width is positive
		let width_value = 
			width_expr.try_eval_ignore_missing(&eval_ctx)?
			.map(|v| v.try_into_i64().ok())
			.flatten();
		
		match width_value {
			Some(w) if w < 1 => return Err(DesignError::InvalidSignalWidth),
			Some(_) => {},
			None => {},
		}

		// Validate array dimension expressions (generic)
		for dim in &self.dimensions {
			dim.validate_no_assumptions(&scope)?;
			let dim_type = dim.eval_type(&eval_ctx)?;
			if !dim_type.sensitivity.is_generic() {
				return Err(DesignError::VariableArrayDimension);
			}

			let dim_value = 
				dim.try_eval_ignore_missing(&eval_ctx)?
				.map(|v| v.try_into_i64().ok())
				.flatten();

			// Check if dimension is positive
			match dim_value {
				Some(w) if w < 1 => return Err(DesignError::InvalidArrayDimension),
				Some(_) => {},
				None => {},
			}
		}

		Ok(())
	}

	/// Creates the signal and adds it to the design. Returns the signal ID.
	pub fn build(self) -> Result<SignalId, DesignError> {
		let sensitivity = match (&self.sensitivity, &self.comb_clocking, &self.sync_clocking) {
			(Some(s), None, None) => s.clone(),
			(None, Some(c), None) => SignalSensitivity::Comb(c.clone()),
			(None, None, Some(s)) => SignalSensitivity::Sync(s.clone()),
			(None, None, None) => return Err(DesignError::SignalSensitivityNotSpecified),
			_ => return Err(DesignError::ConflictingSignalSensitivity),
		};

		// Ensure that class is specified
		self.class.as_ref().ok_or(DesignError::SignalClassNotSpecified)?;

		self.validate()?;
		self.design.borrow_mut().add_signal(Signal::new(
			SignalId { id: 0 },
			self.scope,
			&self.name,
			self.dimensions,
			self.class.expect("Class must be specified"),
			sensitivity,
			self.comment,
			self.is_generated,
		)?)
	}
}
