use core::panic;

use hirn::{
	design::{signal::SignalBuilder, NumericConstant},
	Expression, SignalId,
};
use log::debug;
use num_bigint::BigInt;

use crate::{
	analyzer::report_duplicated_qualifier,
	core::id_table::{self},
	lexer::IdTableKey,
	ProvidesCompilerDiagnostic, SourceSpan,
};

use super::*;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignalSignedness {
	Signed(SourceSpan),
	Unsigned(SourceSpan),
	NoSignedness,
}
impl SignalSignedness {
	pub fn name(&self) -> &'static str {
		use SignalSignedness::*;
		match self {
			Signed(_) => "signed",
			Unsigned(_) => "unsigned",
			NoSignedness => "none",
		}
	}
	pub fn location(&self) -> Option<&SourceSpan> {
		use SignalSignedness::*;
		match self {
			Signed(x) => Some(x),
			Unsigned(x) => Some(x),
			NoSignedness => None,
		}
	}
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Direction {
	Input(SourceSpan),
	Output(SourceSpan),
	Tristate(SourceSpan),
	None,
}
impl Direction {
	pub fn name(&self) -> &'static str {
		use Direction::*;
		match self {
			Input(_) => "input",
			Output(_) => "output",
			Tristate(_) => "tristate",
			None => "none",
		}
	}
	pub fn location(&self) -> Option<&SourceSpan> {
		match self {
			Direction::Input(x) => Some(x),
			Direction::Output(x) => Some(x),
			Direction::Tristate(x) => Some(x),
			Direction::None => None,
		}
	}
}
#[derive(Debug, Clone, Hash, Eq)] // FIXME
pub enum BusWidth {
	Evaluated(crate::core::NumericConstant), // in non generic modules
	EvaluatedLocated(crate::core::NumericConstant, SourceSpan), // in non generic modules
	Evaluable(SourceSpan), // in generic modules
	WidthOf(SourceSpan),  // in generic modules
}
impl PartialEq for BusWidth {
    fn eq(&self, other: &Self) -> bool {
		use BusWidth::*;
        match (self, other) {
            (Evaluated(l0), Evaluated(r0)) => l0 == r0,
            (EvaluatedLocated(l0, _), EvaluatedLocated(r0, _)) => l0 == r0,
            (WidthOf(_), _) => true,
            (BusWidth::Evaluated(l0), BusWidth::EvaluatedLocated(r0, _)) => l0==r0,
            (_, BusWidth::Evaluable(_)) => true,
            (_, BusWidth::WidthOf(_)) => true,
            (BusWidth::EvaluatedLocated(l0, _), BusWidth::Evaluated(r0)) => l0 == r0,
            (BusWidth::Evaluable(_),_) => true,
        }
    }
}
impl BusWidth {
	pub fn to_generic(&mut self) {
		use BusWidth::*;
		match self {
			EvaluatedLocated(_, location) => *self = Evaluable(*location),
			Evaluated(_) => (),
			Evaluable(_) => (),
			WidthOf(_) => (),
		}
	}
	pub fn get_location(&self) -> Option<SourceSpan> {
		use BusWidth::*;
		match self {
			EvaluatedLocated(_, location) => Some(*location),
			Evaluated(_) => None,
			Evaluable(location) => Some(*location),
			WidthOf(location) => Some(*location),
		}
	}
	pub fn get_value(&self) -> Option<BigInt> {
		use BusWidth::*;
		match self {
			Evaluated(value) => Some(value.clone().value),
			EvaluatedLocated(value, _) => Some(value.clone().value),
			Evaluable(_) => None,
			WidthOf(_) => None,
		}
	}
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BusType {
	pub width: Option<BusWidth>,
	pub signedness: SignalSignedness,
	pub location: SourceSpan,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignalType {
	Bus(BusType),
	Wire(SourceSpan),
	Auto(SourceSpan),
}

#[derive(Clone, Debug)]
pub struct AlreadyCreated {
	pub signedness: SignalSignedness,
	pub direction: Direction,
	pub sensitivity: SignalSensitivity,
}

impl AlreadyCreated {
	pub fn new() -> Self {
		Self {
			signedness: SignalSignedness::NoSignedness,
			direction: Direction::None,
			sensitivity: SignalSensitivity::NoSensitivity,
		}
	}
	pub fn add_direction(&mut self, direction: Direction) -> miette::Result<()> {
		use Direction::*;
		match (&self.direction, &direction) {
			(Input(prev), Input(incoming))
			| (Output(prev), Output(incoming))
			| (Tristate(prev), Tristate(incoming)) => report_duplicated_qualifier(&incoming, prev, direction.name())?,
			(None, _) => self.direction = direction,
			(_, None) => (),
			(..) => report_contradicting_qualifier(
				self.direction.location().unwrap(),
				direction.location().unwrap(),
				self.direction.name(),
				direction.name(),
			)?,
		}
		Ok(())
	}
	pub fn add_sensitivity(&mut self, sensitivity: SignalSensitivity) -> miette::Result<()> {
		use SignalSensitivity::*;
		match (&self.sensitivity, &sensitivity) {
			(Async(prev), Async(incoming))
			| (Comb(_, prev), Comb(_, incoming))
			| (Sync(_, prev), Sync(_, incoming))
			| (Clock(prev), Clock(incoming))
			| (Const(prev), Const(incoming)) => report_duplicated_qualifier(incoming, prev, sensitivity.name())?,
			(NoSensitivity, _) => self.sensitivity = sensitivity,
			(_, NoSensitivity) => (),
			(..) => report_contradicting_qualifier(
				self.sensitivity.location().unwrap(),
				sensitivity.location().unwrap(),
				self.sensitivity.name(),
				sensitivity.name(),
			)?,
		};
		Ok(())
	}
	pub fn add_signedness(&mut self, signedness: SignalSignedness) -> miette::Result<()> {
		use SignalSignedness::*;
		match (&self.signedness, &signedness) {
			(Signed(prev), Signed(incoming)) | (Unsigned(prev), Unsigned(incoming)) => {
				report_duplicated_qualifier(incoming, prev, signedness.name())?
			},
			(NoSignedness, _) => self.signedness = signedness,
			(_, NoSignedness) => (),
			(..) => report_contradicting_qualifier(
				self.signedness.location().unwrap(),
				signedness.location().unwrap(),
				self.signedness.name(),
				signedness.name(),
			)?,
		};
		Ok(())
	}
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Signal {
	pub signal_type: SignalType,
	pub dimensions: Vec<BusWidth>,
	pub sensitivity: SignalSensitivity,
	pub direction: Direction,
}
impl Signal {
	pub fn new_bus(width: Option<BusWidth>, signedness: SignalSignedness, location: SourceSpan) -> Self {
		Self {
			signal_type: SignalType::Bus(BusType {
				width,
				signedness,
				location,
			}),
			dimensions: Vec::new(),
			sensitivity: SignalSensitivity::NoSensitivity,
			direction: Direction::None,
		}
	}
	pub fn new_bus_with_sensitivity(width: BusWidth, signedness: SignalSignedness, sensitivity: SignalSensitivity, location: SourceSpan) -> Self {
		Self {
			signal_type: SignalType::Bus(BusType {
				width: Some(width),
				signedness,
				location,
			}),
			dimensions: Vec::new(),
			sensitivity,
			direction: Direction::None,
		}
	}
	pub fn is_wire(&self) -> bool {
		use SignalType::*;
		match &self.signal_type {
			Wire(_) => true,
			_ => false,
		}
	}
	pub fn is_array(&self) -> bool {
		self.dimensions.len() > 0
	}
	pub fn is_bus(&self) -> bool {
		use SignalType::*;
		match &self.signal_type {
			Bus(_) => true,
			_ => false,
		}
	}
	pub fn is_auto(&self) -> bool {
		use SignalType::*;
		match &self.signal_type {
			Auto(_) => true,
			_ => false,
		}
	}
	pub fn new_empty() -> Self {
		Self {
			signal_type: SignalType::Auto(SourceSpan::new_between(0, 0)),
			dimensions: Vec::new(),
			sensitivity: SignalSensitivity::NoSensitivity,
			direction: Direction::None,
		}
	}
	pub fn width(&self) -> Option<BusWidth> {
		use SignalType::*;
		match &self.signal_type {
			Bus(bus) => bus.width.clone(),
			Auto(_) => None,
			Wire(_) => Some(BusWidth::Evaluated(crate::core::NumericConstant::new_true())),
		}
	}
	pub fn set_width(&mut self, width: BusWidth, signedness: SignalSignedness, location: SourceSpan) {
		use SignalType::*;
		match &mut self.signal_type {
			Bus(bus) => bus.width = Some(width),
			Auto(_) => {
				self.signal_type = SignalType::Bus(BusType {
					width: Some(width),
					signedness,
					location,
				})
			},
			Wire(_) => (),
		}
	}
	pub fn get_width_location(&self) -> Option<SourceSpan> {
		use SignalType::*;
		match &self.signal_type {
			Bus(bus) => Some(bus.location),
			Auto(_) => None,
			Wire(location) => Some(*location),
		}
	}
	pub fn new_wire(location: SourceSpan) -> Self {
		Self {
			signal_type: SignalType::Wire(location),
			dimensions: Vec::new(),
			sensitivity: SignalSensitivity::NoSensitivity,
			direction: Direction::None,
		}
	}
	pub fn is_sensititivity_specified(&self) -> bool {
		match &self.sensitivity {
			SignalSensitivity::NoSensitivity => false,
			_ => true,
		}
	}
	pub fn is_direction_specified(&self) -> bool {
		match &self.direction {
			Direction::None => false,
			_ => true,
		}
	}
	pub fn get_signedness(&self) -> SignalSignedness{
		use SignalType::*;
		match &self.signal_type {
			Bus(bus) => bus.signedness.clone(),
			Auto(_) => panic!("Auto type has no signedness"),
			Wire(loc) => SignalSignedness::Unsigned(*loc),
		}
	}
	pub fn is_width_specified(&self) -> bool {
		use SignalType::*;
		match &self.signal_type {
			Bus(bus) => match &bus.width {
				Some(_) => true,
				None => false,
			},
			Auto(_) => false,
			Wire(_) => true,
		}
	}
	/// only if needed
	pub fn is_signedness_specified(&self) -> bool {
		use SignalType::*;
		match &self.signal_type {
			Bus(bus) => {
				use SignalSignedness::*;
				match bus.signedness {
					NoSignedness => false,
					_ => true,
				}
			},
			Auto(_) => false,
			Wire(_) => true,
		}
	}
	pub fn new_from_constant(constant: &crate::core::NumericConstant, location: SourceSpan) -> Self {
		let signedness = match constant.signed {
			Some(value) => {
				if value {
					SignalSignedness::Signed(location)
				}
				else {
					SignalSignedness::Unsigned(location)
				}
			},
			None => SignalSignedness::NoSignedness,
		};
		let width = match constant.width {
			Some(value) => Some(BusWidth::Evaluated(crate::core::NumericConstant::new(BigInt::from(value),None,None,None))),
			None => None, // error FIXME
		};
		if width.clone().unwrap().get_value().unwrap() == 1.into() {
			Self {
				signal_type: SignalType::Wire(location),
				dimensions: Vec::new(),
				sensitivity: SignalSensitivity::NoSensitivity,
				direction: Direction::None,
			}
		}
		else {
		Self {
			signal_type: SignalType::Bus(BusType {
				width,
				signedness,
				location,
			}),
			dimensions: Vec::new(),
			sensitivity: SignalSensitivity::Const(location),
			direction: Direction::None,
		}
	}
	}
	//pub fn combine_two(&mut self, other: &Signal, location: SourceSpan) -> miette::Result<()> {
	//	if self.dimensions != other.dimensions {
	//		return Err(miette::Report::new(
	//			SemanticError::DifferingDimensions
	//			.to_diagnostic_builder()
	//			//.label(self.signal_type.location, "This signal has these dimensions") // FIXME PROPER LABELING
	//			//.label(other.signal_type.location, "This signal has these dimensions")
	//			.build(),
	//		));
	//	}
	//	self.sensitivity = match (&self.sensitivity, &other.sensitivity) {
	//		(SignalSensitivity::Async(_), _) => self.sensitivity.clone(),
	//		(_, SignalSensitivity::Async(_)) => other.sensitivity.clone(),
	//		(SignalSensitivity::Comb(_, _), _) => self.sensitivity.clone(),
	//		(_, SignalSensitivity::Comb(_, _)) => other.sensitivity.clone(),
	//		(SignalSensitivity::Sync(_, _), _) => self.sensitivity.clone(),
	//		(_, SignalSensitivity::Sync(_, _)) => other.sensitivity.clone(),
	//		(SignalSensitivity::Clock(_), _) => self.sensitivity.clone(),
	//		(_, SignalSensitivity::Clock(_)) => other.sensitivity.clone(),
	//		(SignalSensitivity::Const(_), _) => self.sensitivity.clone(),
	//		(_, SignalSensitivity::Const(_)) => other.sensitivity.clone(),
	//		(_, _) => SignalSensitivity::NoSensitivity,
	//	};
	//	match (&self.direction, &other.direction) {
	//		(Direction::Input(_), Direction::Input(_)) => (),
	//		(Direction::Input(_), Direction::Output(_)) => (), // warn
	//		(Direction::Output(_), Direction::Input(_)) => (), // warn
	//		(Direction::Output(_), Direction::Output(_)) => (),
	//		(_, Direction::Tristate(_)) => (),
	//		(Direction::Tristate(_), _) => (),
	//		(_, Direction::None) => (),
	//		(Direction::None, _) => (),
	//	}
	//	use SignalType::*;
	//	self.signal_type = match (&self.signal_type, &other.signal_type) {
	//		(Bus(bus1), Bus(bus2)) => {
	//			let mut new = bus1.clone();
	//			new.signedness = match (&bus1.signedness, &bus2.signedness) {
	//				(SignalSignedness::Signed(_), SignalSignedness::Signed(_))
	//				| (SignalSignedness::Unsigned(_), SignalSignedness::Unsigned(_)) => new.signedness,
	//				(SignalSignedness::Signed(_), SignalSignedness::Unsigned(_))
	//				| (SignalSignedness::Unsigned(_), SignalSignedness::Signed(_)) => todo!(), // report an erro
	//				(_, SignalSignedness::NoSignedness) => new.signedness,
	//				(SignalSignedness::NoSignedness, _) => bus2.signedness.clone(),
	//			};
	//			new.width = match (&bus1.width, &bus2.width) {
	//				(None, None) => None,
	//				(None, Some(_)) => bus2.width.clone(),
	//				(Some(_), None) => bus1.width.clone(),
	//				(Some(val1), Some(val2)) => {
	//					if val1 == val2 {
	//						bus1.width.clone()
	//					} else {
	//						return Err(miette::Report::new(
	//							SemanticError::DifferingBusWidths
	//								.to_diagnostic_builder()
	//								.label(
	//									location,
	//									format!(
	//										"Cannot assign signals - width mismatch. {} bits vs {} bits",
	//										val1, val2
	//									)
	//									.as_str(),
	//								)
	//								.label(bus1.location, "First width specified here")
	//								.label(bus2.location, "Second width specified here")
	//								.build(),
	//						));
	//					}
	//				},
	//			};
	//			SignalType::Bus(new)
	//		},
	//		(Bus(bus), Wire(wire)) => {
	//			return Err(miette::Report::new(
	//				SemanticError::BoundingWireWithBus
	//					.to_diagnostic_builder()
	//					.label(location, "Cannot assign bus to a wire")
	//					.label(*wire, "Signal specified as wire here")
	//					.label(bus.location, "Signal specified as a bus here")
	//					.build(),
	//			));
	//		},
	//		(Wire(wire), Bus(bus)) => {
	//			return Err(miette::Report::new(
	//				SemanticError::BoundingWireWithBus
	//					.to_diagnostic_builder()
	//					.label(location, "Cannot assign signals - type mismatch")
	//					.label(*wire, "Wire type specified here")
	//					.label(bus.location, "Bus type specified here")
	//					.build(),
	//			));
	//		},
	//		(Wire(_), Wire(_)) => self.signal_type.clone(),
	//		(_, _) => todo!(),
	//	};

	//	Ok(())
	//}
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variable {
	pub name: IdTableKey,
	//pub dimensions: Vec<BigInt>,
	/// location of the variable declaration
	pub location: SourceSpan,
	pub kind: VariableKind,
}
impl Variable {
	pub fn is_clock(&self) -> bool {
		match &self.kind {
			VariableKind::Signal(signal) => match &signal.sensitivity {
				SignalSensitivity::Clock(_) => true,
				_ => false,
			},
			VariableKind::Generic(_) => false,
			VariableKind::ModuleInstance(_) => false,
		}
	}
	pub fn register(
		&self,
		nc_table: &crate::lexer::NumericConstantTable,
		id_table: &id_table::IdTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
		mut builder: SignalBuilder,
	) -> miette::Result<SignalId> {
		debug!(
			"registering variable {}\n {:?}",
			id_table.get_by_key(&self.name).unwrap(),
			self
		);
		let id: SignalId;
		match &self.kind {
			VariableKind::Signal(signal) => {
				use SignalSensitivity::*;
				match &signal.sensitivity {
					Async(_) => builder = builder.asynchronous(),
					Comb(list, _) => {
						for edge in &list.list {
							let id = scope.get_api_id(scope_id, &edge.clock_signal).unwrap();
							builder = builder.comb(id, edge.on_rising);
						}
					},
					Sync(list, _) => {
						for edge in &list.list {
							let id = scope.get_api_id(scope_id, &edge.clock_signal).unwrap();
							builder = builder.sync(id, edge.on_rising);
						}
					},
					Clock(_) => builder = builder.clock(),
					Const(_) => builder = builder.constant(),
					NoSensitivity => unreachable!("No sensitivity should not be possible"),
				}
				match &signal.signal_type {
					SignalType::Bus(bus) => {
						use BusWidth::*;
						let width = match &bus.width.clone().unwrap() {
							Evaluated(value) => {
								Expression::Constant(hirn::design::NumericConstant::new_signed(value.clone().value))
							},
							EvaluatedLocated(_, location) => scope
								.evaluated_expressions
								.get(&location)
								.unwrap()
								.codegen(nc_table, id_table, scope_id, scope)?,
							Evaluable(location) => scope
								.evaluated_expressions
								.get(&location)
								.unwrap()
								.codegen(nc_table, id_table, scope_id, scope)?,
							WidthOf(location) => scope 
								.evaluated_expressions
								.get(&location)
								.unwrap()
								.codegen(nc_table, id_table, scope_id, scope)?, //FIXME coming soon
						};
						match bus.signedness {
							SignalSignedness::Signed(_) => builder = builder.signed(width),
							SignalSignedness::Unsigned(_) => builder = builder.unsigned(width),
							SignalSignedness::NoSignedness => unreachable!(), // report an error
						}
					},
					SignalType::Wire(_) => builder = builder.wire(),
					_ => unreachable!("Only bus and wire types are allowed"),
				}
				for dimension in &signal.dimensions {
					use BusWidth::*;
					match &dimension {
						Evaluated(value) => {
							builder = builder
								.array(Expression::from(NumericConstant::new_unsigned(value.clone().value)))
								.unwrap()
						},
						EvaluatedLocated(_, location) => {
							let expr = scope
								.evaluated_expressions
								.get(location)
								.unwrap()
								.codegen(nc_table, id_table, scope_id, scope)?;
							builder = builder.array(expr).unwrap();
						},
						Evaluable(location) => {
							let expr = scope
								.evaluated_expressions
								.get(location)
								.unwrap()
								.codegen(nc_table, id_table, scope_id, scope)?;
							builder = builder.array(expr).unwrap();
						},
						WidthOf(location) => {
							let expr = scope
								.evaluated_expressions
								.get(location)
								.unwrap()
								.codegen(nc_table, id_table, scope_id, scope)?;
							builder = builder.array(expr).unwrap(); // FIXME it should be width of
						}
					}
				}
				id = builder.build().unwrap();
			},
			VariableKind::Generic(generic) => {
				match &generic.kind {
					GenericVariableKind::Int(sign, _) => match &sign {
						SignalSignedness::Unsigned(_) => {
							builder = builder.unsigned(Expression::from(NumericConstant::new_signed(BigInt::from(64))))
						},
						_ => builder = builder.signed(Expression::from(NumericConstant::new_signed(BigInt::from(64)))),
					},
					GenericVariableKind::Bool(_) => {
						builder = builder.wire();
					},
				}
				builder = builder.generic();
				id = builder.build().unwrap();
			},
			VariableKind::ModuleInstance(_) => unreachable!("Module instantion should not be possible here"),
		}
		Ok(id)
	}
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericVariableKind {
	Int(SignalSignedness, SourceSpan),
	Bool(SourceSpan),
}
impl GenericVariableKind {
	pub fn location(&self) -> SourceSpan {
		use GenericVariableKind::*;
		match self {
			Int(_, x) => *x,
			Bool(x) => *x,
		}
	}
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericVariable {
	pub value: Option<BusWidth>,
	pub direction: Direction,
	pub dimensions: Vec<BusWidth>,
	pub kind: GenericVariableKind,
}
impl GenericVariable {
	pub fn is_direction_specified(&self) -> bool {
		use Direction::*;
		match &self.direction {
			Input(_) => true,
			Output(_) => true,
			Tristate(_) => true,
			None => false,
		}
	}
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModuleInstance {
	pub module_name: IdTableKey,
	pub location: SourceSpan,
	pub interface: Vec<Variable>,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VariableKind {
	Signal(Signal),
	Generic(GenericVariable),
	ModuleInstance(ModuleInstance),
}

impl VariableKind {
	pub fn add_value(&mut self, value: BusWidth) {
		use VariableKind::*;
		match self {
			Generic(gen) => gen.value = Some(value),
			_ =>panic!("Only generic variables can have values"),
		}
	}
	pub fn add_dimenstions(&mut self, dimensions: Vec<BusWidth>) {
		match self {
			VariableKind::Signal(signal) => signal.dimensions = dimensions,
			VariableKind::Generic(gen) => gen.dimensions = dimensions,
			VariableKind::ModuleInstance(_) => panic!("Module instantion can't have dimensions"),
		}
	}
	pub fn to_signal(&self) -> Signal {
		match self {
			VariableKind::Signal(signal) => signal.clone(),
			VariableKind::Generic(gen) => {
				match &gen.value {
					None => (),
					Some(val) => {
						todo!();
						//return Signal::new_from_constant(val, gen.kind.location());
					},
				}
				match &gen.kind {
					GenericVariableKind::Int(signedness, location) => Signal::new_bus(
						Some(BusWidth::Evaluated(crate::core::NumericConstant::new(BigInt::from(64), None, None, None))),
						signedness.clone(),
						*location,
					),
					GenericVariableKind::Bool(location) => Signal::new_wire(*location),
				}
			},
			VariableKind::ModuleInstance(_) => todo!(), // ERROR,
		}
	}
}

pub fn report_contradicting_qualifier(
	location_first: &SourceSpan,
	location_second: &SourceSpan,
	name_first: &str,
	name_second: &str,
) -> miette::Result<()> {
	Err(miette::Report::new(
		SemanticError::ContradictingQualifier
			.to_diagnostic_builder()
			.label(
				*location_first,
				format!(
					"This qualifier: \"{}\" contradicts the \"{}\" qualifier.",
					name_first, name_second
				)
				.as_str(),
			)
			.label(
				*location_second,
				format!(
					"This qualifier \"{}\" is mutually exclusive with \"{}\"",
					name_second, name_first
				)
				.as_str(),
			)
			.build(),
	))
}
