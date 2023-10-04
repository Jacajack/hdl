
use hirn::{design::{NumericConstant, ModuleHandle, ScopeHandle, signal::SignalBuilder}, SignalId, Expression};
use log::debug;
use logos::Source;
use num_bigint::BigInt;

use crate::{ ProvidesCompilerDiagnostic, SourceSpan, lexer::IdTableKey, analyzer::report_duplicated_qualifier, core::id_table};

use super::*;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignalSignedness {
	Signed(SourceSpan),
	Unsigned(SourceSpan),
	None,
}
impl SignalSignedness{
	pub fn name(&self) -> &'static str{
		use SignalSignedness::*;
		match self{
			Signed(_) => "signed",
			Unsigned(_) => "unsigned",
			None => "none",
		}
	}
	pub fn location(&self) -> Option<&SourceSpan>{
		match self{
			SignalSignedness::Signed(x) => Some(x),
			SignalSignedness::Unsigned(x) => Some(x),
			SignalSignedness::None => None,
		}
	}

}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Direction {
	Input(SourceSpan),
	Output(SourceSpan),
	Tristate(SourceSpan),
	None
}
impl Direction{
	pub fn name(&self) -> &'static str{
		use Direction::*;
		match self{
			Input(_) => "input",
			Output(_) => "output",
			Tristate(_) => "tristate",
			None => "none",
		}
	}
	pub fn location(&self) -> Option<&SourceSpan>{
		match self{
			Direction::Input(x) => Some(x),
			Direction::Output(x) => Some(x),
			Direction::Tristate(x) => Some(x),
			Direction::None => None,
		}
	}
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BusType {
	pub width: Option<BigInt>,
	pub signedness: SignalSignedness,
	pub location: SourceSpan,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignalType {
	Bus(BusType),
	Wire(SourceSpan)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct EdgeSensitivity {
	pub clock_signal: IdTableKey,
	pub on_rising: bool,
	pub location: SourceSpan,
}

/// Determines sensitivity of a signal to certain clocks
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClockSensitivityList{
	pub list: Vec<EdgeSensitivity>
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SignalSensitivity {
	Async(SourceSpan),
	Comb(ClockSensitivityList, SourceSpan),
	Sync(ClockSensitivityList, SourceSpan),
	Clock(SourceSpan),
	Const(SourceSpan),
	/// if at the end of the analysis this is still None, it is an error
	NoSensitivity
}
impl SignalSensitivity{
	pub fn name(&self) -> &'static str{
		use SignalSensitivity::*;
		match self{
			Async(_) => "async",
			Comb(_, _) => "comb",
			Sync(_, _) => "sync",
			Clock(_) => "clock",
			Const(_) => "const",
			NoSensitivity => "none",
		}
	}
	pub fn location(&self) -> Option<&SourceSpan>{
		match self{
			SignalSensitivity::Async(x) => Some(x),
			SignalSensitivity::Comb(_, x) => Some(x),
			SignalSensitivity::Sync(_, x) => Some(x),
			SignalSensitivity::Clock(x) => Some(x),
			SignalSensitivity::Const(x) => Some(x),
			SignalSensitivity::NoSensitivity => None,
		}
	}
}
#[derive(Clone, Debug)]
pub struct AlreadyCreated{
	pub signedness: SignalSignedness,
	pub direction: Direction,
	pub sensitivity: SignalSensitivity,
}

impl AlreadyCreated{
	pub fn new() -> Self{
		Self{
			signedness: SignalSignedness::None,
			direction: Direction::None,
			sensitivity: SignalSensitivity::NoSensitivity,
		}
	}
	pub fn add_direction(&mut self, direction: Direction)->miette::Result<()>{
		use Direction::*;
		match (&self.direction, &direction){
    		(Input(prev), Input(incoming)) | (Output(prev), Output(incoming)) | (Tristate(prev), Tristate(incoming))  => report_duplicated_qualifier(&incoming, prev, direction.name())?,
    		(None, _) => self.direction = direction,
			(_, None) => (),
    		(_, _) => report_contradicting_qualifier(self.direction.location().unwrap(), direction.location().unwrap(),  self.direction.name(), direction.name())?,
		}
		Ok(())
	}
	pub fn add_sensitivity(&mut self, sensitivity: SignalSensitivity) -> miette::Result<()>{
		use SignalSensitivity::*;
		match (&self.sensitivity, &sensitivity){
    		(Async(prev), Async(incoming)) | (Comb(_, prev), Comb(_, incoming)) | (Sync(_, prev), Sync(_, incoming)) | (Clock(prev), Clock(incoming)) | (Const(prev), Const(incoming)) => report_duplicated_qualifier(incoming, prev, sensitivity.name())?,
    		(NoSensitivity, _)=> self.sensitivity = sensitivity,
    		(_, NoSensitivity) => (),
			(_,_) => report_contradicting_qualifier(self.sensitivity.location().unwrap(), sensitivity.location().unwrap(), self.sensitivity.name(), sensitivity.name())?,

		};
		Ok(())
	}
	pub fn add_signedness(&mut self, signedness: SignalSignedness) ->miette::Result<()>{
		use SignalSignedness::*;
		match (&self.signedness, &signedness){
			(Signed(prev), Signed(incoming)) | (Unsigned(prev), Unsigned(incoming)) => report_duplicated_qualifier(incoming, prev, signedness.name())?,
			(None, _) => self.signedness = signedness,
			(_, None) => (),
			(_, _) => report_contradicting_qualifier(self.signedness.location().unwrap(), signedness.location().unwrap(), self.signedness.name(), signedness.name())?,
		};
		Ok(())
	}
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signal{
	pub signal_type: SignalType,
	pub dimensions: Vec<BigInt>,
	pub sensitivity: SignalSensitivity,
	pub direction: Direction,
}
impl Signal{
	pub fn new_bus(width: Option<BigInt>, signedness: SignalSignedness, location: SourceSpan) -> Self{
		Self{
			signal_type: SignalType::Bus(BusType{
				width,
				signedness,
				location,
			}),
			dimensions: Vec::new(),
			sensitivity: SignalSensitivity::NoSensitivity,
			direction: Direction::None,
		}
	}
	pub fn new_wire(location: SourceSpan) -> Self{
		Self{
			signal_type: SignalType::Wire(location),
			dimensions: Vec::new(),
			sensitivity: SignalSensitivity::NoSensitivity,
			direction: Direction::None,
		}
	}
	pub fn is_sensititivity_specified(&self) -> bool {
		match &self.sensitivity{
			SignalSensitivity::NoSensitivity => false,
			_ => true,
		}
	}
	pub fn is_direction_specified(&self) -> bool {
		match &self.direction{
			Direction::None => false,
			_ => true,
		}
	}
	/// only if needed
	pub fn is_signedness_specified(&self) -> bool {
		match &self.signal_type{
			SignalType::Bus(bus) => {
				match bus.signedness{
					SignalSignedness::None => false,
					_ => true,
				}
			},
			SignalType::Wire(_) => true,
		}
	}
	pub fn new_from_constant(constant: &crate::lexer::NumericConstant, location: SourceSpan) -> Self{
		let signedness = match constant.signed{
			Some(value) => {
				if value{
					SignalSignedness::Signed(location)
				}else{
					SignalSignedness::Unsigned(location)
				}
			}
			None => SignalSignedness::None,
			};
		let width = match constant.width{
			Some(value) => Some(value.into()),
			None => None, // error
		};
		Self{
			signal_type: SignalType::Bus(BusType{
				width,
				signedness,
				location,
			}),
			dimensions: Vec::new(),
			sensitivity: SignalSensitivity::Const(location),
			direction: Direction::None,
		}
	}
	pub fn combine_two(&mut self, other: &Signal, location: SourceSpan) -> miette::Result<()>{
		if self.dimensions != other.dimensions {
			return Err(miette::Report::new(SemanticError::DifferingDimensions
				.to_diagnostic_builder()
				//.label(self.signal_type.location, "This signal has these dimensions") // FIXME PROPER LABELING
				//.label(other.signal_type.location, "This signal has these dimensions")
				.build()));
		}
		self.sensitivity = match (&self.sensitivity, &other.sensitivity) {
    		(SignalSensitivity::Async(_), _) => self.sensitivity.clone(),
    		(_, SignalSensitivity::Async(_)) => other.sensitivity.clone(),
    		(SignalSensitivity::Comb(_, _), _) => self.sensitivity.clone(),
    		(_, SignalSensitivity::Comb(_, _)) => other.sensitivity.clone(),
    		(SignalSensitivity::Sync(_, _), _) => self.sensitivity.clone(),
    		(_, SignalSensitivity::Sync(_, _)) => other.sensitivity.clone(),
    		(SignalSensitivity::Clock(_),_) => self.sensitivity.clone(),
    		(_, SignalSensitivity::Clock(_)) => other.sensitivity.clone(),
    		(SignalSensitivity::Const(_), _) => self.sensitivity.clone(),
    		(_, SignalSensitivity::Const(_)) => other.sensitivity.clone(),
    		(_, _) => SignalSensitivity::NoSensitivity,
		};
		match (&self.direction, &other.direction) {
    		(Direction::Input(_), Direction::Input(_)) => (),
    		(Direction::Input(_), Direction::Output(_)) => (), // warn
    		(Direction::Output(_), Direction::Input(_)) => (), // warn
    		(Direction::Output(_), Direction::Output(_)) => (),
    		(_, Direction::Tristate(_)) => (),
    		(Direction::Tristate(_), _) => (),
    		(_, Direction::None) => (),
			(Direction::None, _) => (),
		}
		self.signal_type = match (&self.signal_type, &other.signal_type) {
    		(SignalType::Bus(bus1), SignalType::Bus(bus2)) => {
				let mut new = bus1.clone();
				new.signedness = match (&bus1.signedness, &bus2.signedness) {
        			(SignalSignedness::Signed(_), SignalSignedness::Signed(_)) |(SignalSignedness::Unsigned(_), SignalSignedness::Unsigned(_)) => new.signedness,
        			(SignalSignedness::Signed(_), SignalSignedness::Unsigned(_)) | (SignalSignedness::Unsigned(_), SignalSignedness::Signed(_)) => todo!(), // report an erro
        			(_, SignalSignedness::None) => new.signedness,
					(SignalSignedness::None, _) => bus2.signedness.clone(),
    			};
				new.width = match (&bus1.width, &bus2.width) {
        			(None, None) => None,
        			(None, Some(_)) => bus2.width.clone(),
        			(Some(_), None) => bus1.width.clone(),
        			(Some(val1), Some(val2)) => {
						if val1 == val2{
							bus1.width.clone()
						} else {
							return Err(miette::Report::new(SemanticError::DifferingBusWidths
								.to_diagnostic_builder()
								//.label(location,"Cannot assign signals - width mismatch")
								.label(location, format!("Cannot assign signals - width mismatch. {} bits vs {} bits", val1, val2).as_str())
								.label(bus1.location, "First width specified here") 
								.label(bus2.location, "Second width specified here")
								.build()));
						}
					}
    			};
				SignalType::Bus(new)
			},
    		(SignalType::Bus(bus), SignalType::Wire(wire)) => 
			{
				return Err(miette::Report::new(SemanticError::BoundingWireWithBus.to_diagnostic_builder()
					.label(location, "Cannot assign bus to a wire")
					.label(*wire, "Signal specified as wire here")
					.label(bus.location, "Signal specified as a bus here")
					.build()));
			},
    		(SignalType::Wire(wire), SignalType::Bus(bus)) => {
				return Err(miette::Report::new(SemanticError::BoundingWireWithBus.to_diagnostic_builder()
				.label(location, "Cannot assign signals - type mismatch")
				.label(*wire, "Wire type specified here")
				.label(bus.location, "Bus type specified here")
				.build()));
			},
    		(SignalType::Wire(_), SignalType::Wire(_)) => self.signal_type.clone(),
		};

		Ok(())
	}
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable{
	pub name: IdTableKey,
	pub dimensions: Vec<BigInt>,
	/// location of the variable declaration
	pub location: SourceSpan,
	pub kind: VariableKind,

}
impl Variable{
	pub fn is_clock(&self) -> bool {
		match &self.kind{
			VariableKind::Signal(signal) => {
				match &signal.sensitivity{
					SignalSensitivity::Clock(_) => true,
					_ => false,
				}
			},
			VariableKind::Generic(_) => false,
		}
	}
	pub fn register(&self,
		id_table: &id_table::IdTable,
		scope_id: usize,
		scope: &ModuleImplementationScope,
		mut builder: SignalBuilder) -> miette::Result<SignalId> {
		debug!("registering variable {}\n {:?}", id_table.get_by_key(&self.name).unwrap(), self);
		builder = builder.name(id_table.get_by_key(&self.name).unwrap());
		let id: SignalId;
		match &self.kind{
    		VariableKind::Signal(signal) => {
				use SignalSensitivity::*;
				match &signal.sensitivity{
        			Async(_) => builder = builder.asynchronous(),
        			Comb(list, _) => {
						for edge in &list.list{
							let id = scope.get_api_id(scope_id, &edge.clock_signal).unwrap();
							builder = builder.comb(id,edge.on_rising);
						}
					},
        			Sync(list, _) => {
						for edge in &list.list{
							let id = scope.get_api_id(scope_id, &edge.clock_signal).unwrap();
							builder = builder.sync(id, edge.on_rising);
						}
					},
        			Clock(_) => builder = builder.clock(),
        			Const(_) => builder = builder.constant(),
        			NoSensitivity => unreachable!("No sensitivity should not be possible")
    			}
				match &signal.signal_type{
        			SignalType::Bus(bus) => {
						match bus.signedness{
							SignalSignedness::Signed(_) => builder = builder.signed(hirn::Expression::from(NumericConstant::new_signed(bus.width.clone().unwrap().clone()))), // FIXME
							SignalSignedness::Unsigned(_) => builder = builder.unsigned(hirn::Expression::from(NumericConstant::new_signed(bus.width.clone().unwrap().clone()))), // FIXME
							SignalSignedness::None => unreachable!(), // report an error
						}
					},
        			SignalType::Wire(_) => builder = builder.wire(),
    			}
				for dimension in &signal.dimensions{
					builder = builder.array(Expression::from(NumericConstant::new_unsigned(dimension.clone()))).unwrap();
				}
				id = builder.build().unwrap();
			},
    		VariableKind::Generic(generic) => {
				match &generic.kind{
					GenericVariableKind::Auto(_) => unreachable!(), // FIXME report an error
					GenericVariableKind::Int(sign,_) => {
						match &sign{
        					SignalSignedness::Unsigned(_) => builder = builder.unsigned(Expression::from(NumericConstant::new_signed(BigInt::from(64)))),
        					_ => builder = builder.signed(Expression::from(NumericConstant::new_signed(BigInt::from(64)))),
    					}
						
					},
					GenericVariableKind::Bool(_) =>{
						builder = builder.wire();
					} 
				}
				builder = builder.constant();
				id = builder.build().unwrap();
			},
		}
		Ok(id)
	}
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GenericVariableKind{
	Auto(SourceSpan),
	Int(SignalSignedness,SourceSpan),
	Bool(SourceSpan),
}
impl GenericVariableKind {
	pub fn location(&self) -> SourceSpan {
		use GenericVariableKind::*;
		match self {
			Auto(x) => *x,
			Int(_, x) => *x,
			Bool(x) => *x,
		}
	}
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenericVariable{
	pub value: Option<crate::lexer::NumericConstant>,
	pub dimensions: Vec<BigInt>,
	pub kind: GenericVariableKind
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariableKind{
	Signal(Signal),
	Generic(GenericVariable),
}

impl VariableKind {
	pub fn to_signal(&self) -> Option<Signal> {
		match self {
			VariableKind::Signal(signal) => Some(signal.clone()),
			VariableKind::Generic(gen) => {
				match &gen.value{
					None => (),
					Some(val) => {
						return Some(Signal::new_from_constant(val, gen.kind.location()));
					}
				}
				match &gen.kind{
        			GenericVariableKind::Auto(_) => None,
        			GenericVariableKind::Int(signedness, location) => Some(Signal::new_bus(Some(BigInt::from(64)), signedness.clone(), *location)),
        			GenericVariableKind::Bool(location) => Some(Signal::new_wire(*location)),
    			}
			},
		}
	}
}
//#[derive(Clone, Debug, PartialEq, Eq)]
//pub struct CombinedQualifiers {
//	pub signed: Option<SourceSpan>,
//	pub unsigned: Option<SourceSpan>,
//	pub constant: Option<SourceSpan>,
//	pub comb: Option<(Vec<Expression>, SourceSpan)>,
//	pub input: Option<SourceSpan>,
//	pub output: Option<SourceSpan>,
//	pub clock: Option<SourceSpan>,
//	pub asynchronous: Option<SourceSpan>,
//	pub synchronous: Option<(Vec<Expression>, SourceSpan)>,
//}

//impl CombinedQualifiers {
//	pub fn new() -> Self {
//		Self {
//			signed: None,
//			unsigned: None,
//			constant: None,
//			comb: None,
//			input: None,
//			output: None,
//			clock: None,
//			asynchronous: None,
//			synchronous: None,
//		}
//	}
//	pub fn check_for_contradicting(&self) -> miette::Result<()> {
//		match self {
//			CombinedQualifiers {
//				signed: Some(x),
//				unsigned: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "signed", "unsigned")?;
//			},
//			CombinedQualifiers {
//				input: Some(x),
//				output: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "input", "output")?;
//			},
//			CombinedQualifiers {
//				constant: Some(x),
//				comb: Some((_, y)),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "const", "comb")?;
//			},
//			CombinedQualifiers {
//				constant: Some(x),
//				synchronous: Some((_, y)),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "const", "sync")?;
//			},
//			CombinedQualifiers {
//				constant: Some(x),
//				asynchronous: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "const", "async")?;
//			},
//			CombinedQualifiers {
//				constant: Some(x),
//				clock: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "const", "clock")?;
//			},
//			CombinedQualifiers {
//				comb: Some((_, x)),
//				synchronous: Some((_, y)),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "comb", "sync")?;
//			},
//			CombinedQualifiers {
//				comb: Some((_, x)),
//				asynchronous: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "comb", "async")?;
//			},
//			CombinedQualifiers {
//				comb: Some((_, x)),
//				clock: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "comb", "clock")?;
//			},
//			CombinedQualifiers {
//				synchronous: Some((_, x)),
//				clock: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "sync", "clock")?;
//			},
//			CombinedQualifiers {
//				synchronous: Some((_, x)),
//				asynchronous: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "sync", "async")?;
//			},
//			CombinedQualifiers {
//				asynchronous: Some(x),
//				clock: Some(y),
//				..
//			} => {
//				report_contradicting_qualifier(x, y, "async", "clock")?;
//			},
//			_ => (),
//		};
//		Ok(())
//	}
//}
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
