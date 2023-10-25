use core::panic;
use std::{collections::HashMap, hash::Hash};

use hirn::design::{Expression, NumericConstant, SignalBuilder, SignalId};
use log::debug;
use num_bigint::BigInt;

use crate::{
	analyzer::report_duplicated_qualifier,
	core::{
		id_table::{self, IdTable},
		CompilerDiagnosticBuilder,
	},
	lexer::IdTableKey,
	ProvidesCompilerDiagnostic, SourceSpan,
};

use super::{module_implementation_scope::InternalVariableId, *};
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignalSignedness {
	Signed(SourceSpan),
	Unsigned(SourceSpan),
	NoSignedness,
}
impl SignalSignedness {
	pub fn is_none(&self) -> bool {
		use SignalSignedness::*;
		match self {
			NoSignedness => true,
			_ => false,
		}
	}
	pub fn is_signed(&self) -> bool {
		use SignalSignedness::*;
		match self {
			Signed(_) => true,
			_ => false,
		}
	}
	pub fn is_unsigned(&self) -> bool {
		use SignalSignedness::*;
		match self {
			Unsigned(_) => true,
			_ => false,
		}
	}
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
	Evaluable(SourceSpan),                   // in generic modules
	WidthOf(SourceSpan),                     // in generic modules
}
impl PartialEq for BusWidth {
	fn eq(&self, other: &Self) -> bool {
		use BusWidth::*;
		match (self, other) {
			(Evaluated(l0), Evaluated(r0)) => l0 == r0,
			(EvaluatedLocated(l0, _), EvaluatedLocated(r0, _)) => l0 == r0,
			(WidthOf(_), _) => true,
			(BusWidth::Evaluated(l0), BusWidth::EvaluatedLocated(r0, _)) => l0 == r0,
			(_, BusWidth::Evaluable(_)) => true,
			(_, BusWidth::WidthOf(_)) => true,
			(BusWidth::EvaluatedLocated(l0, _), BusWidth::Evaluated(r0)) => l0 == r0,
			(BusWidth::Evaluable(_), _) => true,
		}
	}
}
impl BusWidth {
	pub fn is_located(&self) -> bool {
		use BusWidth::*;
		match self {
			EvaluatedLocated(..) => true,
			Evaluated(_) => false,
			Evaluable(_) => true,
			WidthOf(_) => true,
		}
	}
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
	pub fn eval(
		&mut self,
		nc_table: &crate::lexer::NumericConstantTable,
		id_table: &IdTable,
		scope: &ModuleImplementationScope,
	) -> miette::Result<()> {
		// FIXME
		use BusWidth::*;
		match self {
			Evaluated(_) => (),
			EvaluatedLocated(nc, _) => *self = BusWidth::Evaluated(nc.clone()),
			Evaluable(location) => {
				let expr = scope.evaluated_expressions.get(location).unwrap();
				debug!("Expr is known!");
				let nc = expr.expression.evaluate(&nc_table, expr.scope_id, scope)?.unwrap(); // FIXME
				if nc.value < 0.into() {
					return Err(miette::Report::new(
						SemanticError::NegativeBusWidth
							.to_diagnostic_builder()
							.label(*location, "Bus width must be positive")
							.label(*location, format!("Actual width: {:?}", nc.value).as_str())
							.build(),
					));
				}
				*self = BusWidth::Evaluated(nc)
			},
			WidthOf(location) => {
				//let expr = scope.evaluated_expressions.get(location).unwrap().evaluate_type(&nc_table, 0, scope)?.unwrap();
			},
		}
		Ok(())
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
	pub fn get_nc(&self) -> crate::core::NumericConstant{
		use BusWidth::*;
		match self {
			Evaluated(value) => value.clone(),
			EvaluatedLocated(value, _) => value.clone(),
			Evaluable(_) => panic!("Cannot get numeric constant from an unevaluated expression"),
			WidthOf(_) => panic!("Cannot get numeric constant from an unevaluated expression"),
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
			| (Clock(prev, _), Clock(incoming, _))
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signal {
	pub signal_type: SignalType,
	pub dimensions: Vec<BusWidth>,
	pub sensitivity: SignalSensitivity,
	pub direction: Direction,
}
impl Signal {
	pub fn translate_clocks(&mut self, clocks: &HashMap<IdTableKey, IdTableKey>) {
		use SignalSensitivity::*;
		match &mut self.sensitivity {
			Comb(list, id) => {
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
	pub fn get_clock_name(&self) -> IdTableKey {
		use SignalSensitivity::*;
		match &self.sensitivity {
			Clock(_, Some(name)) => *name,
			_ => panic!("This signal is not a clock"),
		}
	}
	pub fn evaluate_as_lhs(
		&mut self,
		is_lhs: bool,
		global_ctx: &GlobalAnalyzerContext,
		coupling_type: Signal,
		location: SourceSpan,
	) -> miette::Result<()> {
		if is_lhs {
			self.sensitivity
				.can_drive(&coupling_type.sensitivity, location, global_ctx)?;
		}
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
	pub fn set_signedness(&mut self, signedness: SignalSignedness, location: SourceSpan) {
		use SignalType::*;
		match &mut self.signal_type {
			Bus(bus) => bus.signedness = signedness,
			Wire(_) => panic!("You cannot set signedness on a wire"),
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
		if width.clone().unwrap().get_value().unwrap() == 1.into() {
			Self {
				signal_type: SignalType::Wire(location),
				dimensions: Vec::new(),
				sensitivity: SignalSensitivity::Const(location),
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
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
	pub name: IdTableKey,
	/// location of the variable declaration
	pub location: SourceSpan,
	pub kind: VariableKind,
}
impl Variable {
	pub fn new(name: IdTableKey, location: SourceSpan, kind: VariableKind) -> Self {
		Self { name, location, kind }
	}
	pub fn is_clock(&self) -> bool {
		match &self.kind {
			VariableKind::Signal(signal) => match &signal.sensitivity {
				SignalSensitivity::Clock(..) => true,
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
		nc_widths: Option<&HashMap<SourceSpan, crate::core::NumericConstant>>,
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
					Clock(..) => builder = builder.clock(),
					Const(_) => builder = builder.constant(),
					NoSensitivity => unreachable!("No sensitivity should not be possible"),
				}
				match &signal.signal_type {
					SignalType::Bus(bus) => {
						use BusWidth::*;
						debug!("Width is {:?}", bus.width.clone().unwrap());
						let width = match &bus.width.clone().unwrap() {
							Evaluated(value) => {
								Expression::Constant(hirn::design::NumericConstant::new_signed(value.clone().value))
							},
							EvaluatedLocated(_, location) => {
								let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
								expr_ast
									.expression
									.codegen(nc_table, id_table, expr_ast.scope_id, scope, nc_widths)?
							},
							Evaluable(location) => {
								let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
								expr_ast
									.expression
									.codegen(nc_table, id_table, expr_ast.scope_id, scope, nc_widths)?
							},
							WidthOf(location) => {
								let expr_ast = scope.evaluated_expressions.get(&location).unwrap();
								expr_ast
									.expression
									.codegen(nc_table, id_table, expr_ast.scope_id, scope, nc_widths)?
							}, //FIXME coming soon
						};
						debug!("Width is {:?}", width);
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
							let expr = scope.evaluated_expressions.get(location).unwrap();
							let codegened =
								expr.expression
									.codegen(nc_table, id_table, expr.scope_id, scope, nc_widths)?;
							builder = builder.array(codegened).unwrap();
						},
						Evaluable(location) => {
							let expr = scope
								.evaluated_expressions
								.get(location)
								.unwrap()
								.expression
								.codegen(nc_table, id_table, scope_id, scope, nc_widths)?;
							builder = builder.array(expr).unwrap();
						},
						WidthOf(location) => {
							let expr = scope
								.evaluated_expressions
								.get(location)
								.unwrap()
								.expression
								.codegen(nc_table, id_table, scope_id, scope, nc_widths)?;
							builder = builder.array(expr).unwrap(); // FIXME it should be width of
						},
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
#[derive(Clone, Debug, PartialEq, Eq)]
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
#[derive(Clone, Debug, PartialEq, Eq)]
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RegisterInstance {
	pub name: IdTableKey,
	pub location: SourceSpan,
	pub next: InternalVariableId,
	pub clk: InternalVariableId,
	pub nreset: InternalVariableId,
	pub data: InternalVariableId,
	pub enable: InternalVariableId,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonRegister {
	pub interface: HashMap<IdTableKey, InternalVariableId>,
}

impl NonRegister {
	pub fn new() -> Self {
		Self {
			interface: HashMap::new(),
		}
	}
	pub fn add_variable(&mut self, name: IdTableKey, var: InternalVariableId) -> Result<(), CompilerDiagnosticBuilder> {
		match self.interface.insert(name, var) {
			Some(_) => Err(SemanticError::DuplicateVariableDeclaration.to_diagnostic_builder()),
			None => Ok(()),
		}
	}
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ModuleInstanceKind {
	Module(NonRegister),
	Register(RegisterInstance),
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleInstance {
	pub module_name: IdTableKey,
	pub location: SourceSpan,
	pub kind: ModuleInstanceKind,
}
impl ModuleInstance {
	pub fn new(module_name: IdTableKey, location: SourceSpan) -> Self {
		Self {
			module_name,
			location,
			kind: ModuleInstanceKind::Module(NonRegister::new()),
		}
	}
	//pub fn add_variable(&mut self, var: Variable) -> Result<(), SemanticError> {
	//	match self.kind {
	//		ModuleInstanceKind::Module(ref mut non_reg) => non_reg.add_variable(var),
	//		ModuleInstanceKind::Register(_) => panic!("Register instance have others method of adding variables"),
	//	}
	//}
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariableKind {
	Signal(Signal),
	Generic(GenericVariable),
	ModuleInstance(ModuleInstance),
}

impl VariableKind {
	/// only if needed
	pub fn add_name_to_clock(&mut self, id: IdTableKey) {
		use VariableKind::*;
		match self {
			Signal(sig) => match &mut sig.sensitivity {
				SignalSensitivity::Clock(..) => {
					sig.sensitivity = SignalSensitivity::Clock(sig.sensitivity.location().unwrap().clone(), Some(id))
				},
				_ => (),
			},
			_ => (),
		}
	}
	pub fn evaluate_bus_width(
		&mut self,
		scope: &ModuleImplementationScope,
		id_table: &IdTable,
		nc_table: &crate::lexer::NumericConstantTable,
	) -> miette::Result<()> {
		use VariableKind::*;
		match self {
			Signal(sig) => {
				use SignalType::*;
				match &mut sig.signal_type {
					Bus(bus) => match &mut bus.width {
						Some(b) => {
							b.eval(nc_table, id_table, scope)?;
						},
						None => (),
					},
					_ => (),
				}
			},
			_ => unreachable!(),
		}
		Ok(())
	}
	pub fn is_generic(&self) -> bool {
		use VariableKind::*;
		match self {
			Generic(_) => true,
			_ => false,
		}
	}
	pub fn is_array(&self) -> bool {
		use VariableKind::*;
		match self {
			Signal(sig) => sig.is_array(),
			Generic(gen) => gen.dimensions.len() > 0,
			_ => false,
		}
	}
	pub fn add_value(&mut self, value: BusWidth) {
		use VariableKind::*;
		match self {
			Generic(gen) => gen.value = Some(value),
			_ => panic!("Only generic variables can have values"),
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
						return Signal::new_from_constant(&val.get_nc(), SourceSpan::new_between(0,0))
						//return Signal::new_from_constant(val, gen.kind.location());
					},
				}
				match &gen.kind {
					GenericVariableKind::Int(signedness, location) => Signal::new_bus(
						Some(BusWidth::Evaluated(crate::core::NumericConstant::new(
							BigInt::from(64),
							None,
							None,
							None,
						))),
						signedness.clone(),
						*location,
					),
					GenericVariableKind::Bool(location) => Signal::new_wire(*location),
				}
			},
			VariableKind::ModuleInstance(_) => todo!(), // ERROR,
		}
	}
	pub fn is_module_instance(&self) -> bool {
		use VariableKind::*;
		match self {
			ModuleInstance(_) => true,
			_ => false,
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
