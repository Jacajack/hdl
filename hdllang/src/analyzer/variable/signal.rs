use crate::analyzer::{
	variable::InternalVariableId, BusWidth, Direction, GlobalAnalyzerContext, SemanticError, SignalSensitivity,
	SignalSignedness,
};
use crate::core::SourceSpan;
use crate::ProvidesCompilerDiagnostic;
use log::*;
use num_bigint::BigInt;
use std::collections::HashMap;

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signal {
	pub signal_type: SignalType,
	pub dimensions: Vec<BusWidth>,
	pub sensitivity: SignalSensitivity,
	pub direction: Direction,
}
impl Signal {
	pub fn translate_clocks(&mut self, clocks: &HashMap<InternalVariableId, InternalVariableId>) {
		use SignalSensitivity::*;
		match &mut self.sensitivity {
			Comb(list, _) => {
				for edge in &mut list.list {
					if let Some(new_id) = clocks.get(&edge.clock_signal) {
						edge.clock_signal = *new_id;
					}
				}
			},
			Sync(list, _) => {
				for edge in &mut list.list {
					if let Some(new_id) = clocks.get(&edge.clock_signal) {
						edge.clock_signal = *new_id;
					}
				}
			},
			_ => (),
		}
	}
	pub fn is_clock(&self) -> bool {
		use SignalSensitivity::*;
		match &self.sensitivity {
			Clock(..) => true,
			_ => false,
		}
	}
	pub fn get_clock_name(&self) -> InternalVariableId {
		use SignalSensitivity::*;
		match &self.sensitivity {
			Clock(_, Some(name)) => *name,
			_ => panic!("This signal is not a clock"),
		}
	}
	pub fn evaluate_as_lhs(
		&mut self,
		_is_lhs: bool,
		_global_ctx: &GlobalAnalyzerContext,
		coupling_type: Signal,
		location: SourceSpan,
	) -> miette::Result<()> {
		//if is_lhs {
		//	self.sensitivity
		//		.can_drive(&coupling_type.sensitivity, location, global_ctx)?;
		//}
		use crate::analyzer::SignalType::*;
		self.signal_type = match (&self.signal_type, &coupling_type.signal_type) {
			(Auto(_), Auto(_)) => self.signal_type.clone(),
			(Auto(_), _) => coupling_type.signal_type.clone(),
			(Bus(bus), Bus(bus1)) => {
				use crate::analyzer::SignalSignedness::*;
				let mut new: crate::analyzer::BusType = bus1.clone();
				new.signedness = match (&bus.signedness, &bus1.signedness) {
					(Signed(_), Signed(_)) | (Unsigned(_), Unsigned(_)) => bus.signedness.clone(),
					(Signed(loc1), Unsigned(loc2)) | (Unsigned(loc1), Signed(loc2)) => {
						return Err(miette::Report::new(
							SemanticError::SignednessMismatch
								.to_diagnostic_builder()
								.label(location, "Cannot assign signals - signedness mismatch")
								.label(*loc2, "Signedness of this signal")
								.label(*loc1, "Signedness of this expression")
								.build(),
						))
					},
					(_, NoSignedness) => bus.signedness.clone(),
					(NoSignedness, _) => bus1.signedness.clone(),
				};
				new.width = match (&bus.width, &bus1.width) {
					(_, None) => bus.width.clone(),
					(None, Some(_)) => bus1.width.clone(),
					(Some(coming), Some(original)) => {
						match (&coming.get_value(), &original.get_value()) {
							(Some(val1), Some(val2)) => {
								if val1 != val2 {
									return Err(miette::Report::new(
										SemanticError::DifferingBusWidths
											.to_diagnostic_builder()
											.label(
												location,
												format!(
													"Cannot assign signals - width mismatch. {} bits vs {} bits",
													val2, val1
												)
												.as_str(),
											)
											.label(bus1.location, "First width specified here")
											.label(bus.location, "Second width specified here")
											.build(),
									));
								}
							},
							_ => (),
						}
						bus.width.clone()
					},
				};
				SignalType::Bus(new)
			},
			(Bus(bus), Wire(wire)) | (Wire(wire), Bus(bus)) => {
				if bus.width.is_none() {
					SignalType::Wire(*wire)
				}
				else {
					debug!("Bus width is {:?}", bus.width.clone().unwrap().get_value().unwrap());
					if bus.width.clone().unwrap().get_value().unwrap() != 1.into() {
						return Err(miette::Report::new(
							SemanticError::BoundingWireWithBus
								.to_diagnostic_builder()
								.label(location, "Cannot assign bus to a wire and vice versa")
								.label(*wire, "Signal specified as wire here")
								.label(bus.location, "Signal specified as a bus here")
								.build(),
						));
					}
					else {
						self.signal_type.clone()
					}
				}
			},
			(Bus(_), Auto(_)) => self.signal_type.clone(),
			(Wire(_), _) => self.signal_type.clone(),
		};
		Ok(())
	}
	pub fn new_empty_with_sensitivity(sensitivity: SignalSensitivity) -> Self {
		Self {
			signal_type: SignalType::Auto(SourceSpan::new_between(0, 0)),
			dimensions: Vec::new(),
			sensitivity,
			direction: Direction::None,
		}
	}
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
	pub fn new_bus_with_sensitivity(
		width: BusWidth,
		signedness: SignalSignedness,
		sensitivity: SignalSensitivity,
		location: SourceSpan,
	) -> Self {
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
	pub fn get_signedness(&self) -> SignalSignedness {
		use SignalType::*;
		match &self.signal_type {
			Bus(bus) => bus.signedness.clone(),
			Auto(_) => SignalSignedness::NoSignedness,
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
	pub fn set_signedness(&mut self, signedness: SignalSignedness, location: SourceSpan) {
		use SignalType::*;
		match &mut self.signal_type {
			Bus(bus) => bus.signedness = signedness,
			Wire(_) => (),
			Auto(_) => {
				self.signal_type = SignalType::Bus(BusType {
					width: None,
					signedness,
					location,
				})
			},
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
			Some(value) => Some(BusWidth::Evaluated(crate::core::NumericConstant::new(
				BigInt::from(value),
				None,
				None,
				None,
			))),
			None => {
				return Self {
					signal_type: SignalType::Bus(BusType {
						width: None,
						signedness,
						location,
					}),
					dimensions: Vec::new(),
					sensitivity: SignalSensitivity::Const(location),
					direction: Direction::None,
				}
			},
		};
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
