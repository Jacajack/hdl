use crate::{analyzer::SemanticError, core::CompilerDiagnosticBuilder, ProvidesCompilerDiagnostic, SourceSpan};

use super::{module_implementation_scope::InternalVariableId, GlobalAnalyzerContext};

#[derive(Clone, Copy, Eq, Debug, Hash, PartialOrd, Ord)]
pub struct EdgeSensitivity {
	pub clock_signal: InternalVariableId,
	pub on_rising: bool,
	pub location: SourceSpan,
}

impl PartialEq for EdgeSensitivity {
	fn eq(&self, other: &Self) -> bool {
		self.clock_signal == other.clock_signal && self.on_rising == other.on_rising
	}
}
/// Determines sensitivity of a signal to certain clocks
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ClockSensitivityList {
	pub list: Vec<EdgeSensitivity>,
}
impl ClockSensitivityList {
	pub fn new() -> Self {
		Self { list: Vec::new() }
	}
	pub fn contains_clock(&self, id: InternalVariableId) -> bool {
		for edge in &self.list {
			if edge.clock_signal == id {
				return true;
			}
		}
		false
	}
	pub fn with_clock(mut self, id: InternalVariableId, on_rising: bool, location: SourceSpan) -> Self {
		self.add_clock(EdgeSensitivity {
			clock_signal: id,
			on_rising,
			location,
		});
		self
	}
	pub fn add_clock(&mut self, clk: EdgeSensitivity) {
		if !self.contains_clock(clk.clock_signal) {
			self.list.push(clk);
		}
	}
	pub fn combine_two(&self, other: &ClockSensitivityList) -> Self {
		let mut result = self.clone();
		for edge in &other.list {
			result.add_clock(*edge);
		}
		result
	}
	pub fn to_ids(&self) -> Vec<InternalVariableId> {
		self.list.iter().map(|x| x.clock_signal).collect()
	}
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SignalSensitivity {
	Async(SourceSpan),
	Comb(ClockSensitivityList, SourceSpan),
	Sync(ClockSensitivityList, SourceSpan),
	Clock(SourceSpan, Option<InternalVariableId>),
	Const(SourceSpan),
	/// if at the end of the analysis this is still None, it is an error
	NoSensitivity,
}
impl SignalSensitivity {
	pub fn name(&self) -> &'static str {
		use SignalSensitivity::*;
		match self {
			Async(_) => "async",
			Comb(..) => "comb",
			Sync(..) => "sync",
			Clock(..) => "clock",
			Const(_) => "const",
			NoSensitivity => panic!("No sensitivity"),
		}
	}
	pub fn is_none(&self) -> bool {
		use SignalSensitivity::*;
		match self {
			NoSensitivity => true,
			_ => false,
		}
	}
	pub fn is_clock(&self) -> bool {
		use SignalSensitivity::*;
		match self {
			Clock(..) => true,
			_ => false,
		}
	}
	pub fn evaluate_sensitivity(&mut self, others: Vec<SignalSensitivity>, location: SourceSpan) {
		use SignalSensitivity::*;
		for sens in others {
			match (&self, &sens) {
				(Async(_), _) => return,
				(_, Async(_)) => *self = sens.clone(),
				(Comb(l1, _), Comb(l2, _)) => *self = Comb(l1.combine_two(l2), location),
				(Comb(l1, _), Sync(l2, _)) => *self = Comb(l1.combine_two(l2), location),
				(Comb(l, _), Clock(loc, Some(id))) => {
					let mut new_list = l.clone();
					new_list.add_clock(EdgeSensitivity {
						clock_signal: *id,
						on_rising: true,
						location: *loc,
					});
					*self = Comb(new_list, location);
				},
				(Clock(loc, Some(id)), Comb(l, _)) => {
					let mut new_list = l.clone();
					new_list.add_clock(EdgeSensitivity {
						clock_signal: *id,
						on_rising: true,
						location: *loc,
					});
					*self = Comb(new_list, location);
				},
				(Comb(..), Const(_)) => (),
				(_, NoSensitivity) => (),
				(Sync(l1, _), Comb(l2, _)) => *self = Comb(l1.combine_two(l2), location),
				(Sync(l1, _), Sync(l2, _)) => {
					if l1 == l2 {
						*self = Sync(l1.clone(), location);
					}
					else {
						*self = Comb(l1.combine_two(l2), location);
					}
				},
				(Sync(..), Clock(..)) => *self = sens.clone(),
				(Sync(..), Const(_)) => (),
				(Clock(loc1, Some(id1)), Clock(loc2, Some(id2))) => {
					let list = ClockSensitivityList::new()
						.with_clock(*id1, true, *loc1)
						.with_clock(*id2, true, *loc2);
					*self = Comb(list, location);
				},
				(Clock(..), _) => (),
				(Const(_), Const(_)) => (),
				(Const(_), _) => *self = sens.clone(),
				(NoSensitivity, _) => *self = sens.clone(),
				_ => (),
			}
		}
	}
	pub fn location(&self) -> Option<&SourceSpan> {
		use SignalSensitivity::*;
		match self {
			Async(x) => Some(x),
			Comb(_, x) => Some(x),
			Sync(_, x) => Some(x),
			Clock(x, _) => Some(x),
			Const(x) => Some(x),
			NoSensitivity => None,
		}
	}
	pub fn is_not_worse_than(&self, other: &SignalSensitivity) -> bool {
		use SignalSensitivity::*;
		match (self, other) {
			(_, Async(_)) => true,
			(Async(_), _) => false,
			(Comb(l1, _), Comb(l2, _)) => {
				for edge in &l1.list {
					if !l2.contains_clock(edge.clock_signal) {
						return false;
					}
				}
				true
			},
			(Comb(l1, _), Sync(l2, _)) => {
				for edge in &l1.list {
					if !l2.contains_clock(edge.clock_signal) {
						return false;
					}
				}
				true
			},
			(Comb(l1, _), Clock(_, Some(name))) => {
				if l1.contains_clock(name.clone()) {
					return true;
				}
				false
			},
			(Comb(..), _) => false,
			_ => true,
		}
	}
	/// DEPRECATED
	pub fn can_drive(
		&mut self,
		rhs: &SignalSensitivity,
		location: SourceSpan,
		_global_ctx: &GlobalAnalyzerContext,
	) -> miette::Result<()> {
		use SignalSensitivity::*;
		log::debug!("Self {:?}", self);
		log::debug!("Other {:?}", rhs);
		match (&self, rhs) {
			(_, NoSensitivity)
			| (Async(_), Async(_))
			| (Sync(..), Sync(..))
			| (Const(_), Const(_))
			| (Clock(..), Clock(..)) => (),
			(NoSensitivity, _) => *self = rhs.clone(),
			(Comb(curent, _), Comb(incoming, _)) => {
				for value in &incoming.list {
					if !curent.contains_clock(value.clock_signal) {
						return Err(miette::Report::new(
							SemanticError::DifferingSensitivities
								.to_diagnostic_builder()
								.label(
									location,
									"Cannot assign signals - sensitivity mismatch. Sensitivty of the left hand side should be a super set of the right hand side",
								)
								//.label(*lhs_location, format!("This sensitivity list does not contain this clock {:?}", global_ctx.id_table.get_by_key(&value.clock_signal).unwrap() ).as_str())
								//.label(value.location, format!("This clock {:?} is not present in left hand side sensitivity list", global_ctx.id_table.get_by_key(&value.clock_signal).unwrap() ).as_str())
								.build(),
						));
					}
				}
			},
			(_, Async(sensitivity_location)) => {
				return Err(miette::Report::new(
					SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(*self.location().unwrap(), "This sensitivity is better than asynchronous")
						.label(
							location,
							"Cannot assign signals - sensitivity mismatch. Sensitivity of the land hand side should be worse or the same as the right hand side",
						)
						.label(*sensitivity_location, "This sensitivity is asynchronous")
						.build(),
				));
			},
			(Async(_), _) => (),
			(_, Comb(_, sensitivity_location)) => {
				return Err(miette::Report::new(
					SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(
							location,
							"Cannot assign signals - sensitivity mismatch. Sensitivity of the land hand side should be worse or the same as the right hand side",
						)
						.label(*sensitivity_location, "This sensitivity is better than comb")
						.build(),
				));
			},
			(Comb(..), _) => (),
			(_, Sync(_, sensitivity_location)) => {
				return Err(miette::Report::new(
					SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(
							location,
							"Cannot assign signals - sensitivity mismatch. Sensitivity of the land hand side should be worse or the same as the right hand side",
						)
						.label(*sensitivity_location, "This sensitivity is better than sync")
						.build(),
				));
			},
			(Sync(..), _) => (),
			(_, Const(_)) => (),
			(Const(sensitivity_location), _) => {
				return Err(miette::Report::new(
					SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(
							location,
							"Cannot bind signals - sensitivity mismatch. Sensitivity on the left assignment side must be worse or same as on the right hand side",
						)
						.label(*sensitivity_location, "This sensitivity is const")
						.label(*rhs.location().unwrap(), "This sensitivity is worse than const")
						.build(),
				));
			},
		}
		Ok(())
	}
	pub fn can_drive_report_as_builder(
		&self,
		rhs: &SignalSensitivity,
		location: SourceSpan,
		scope: &crate::analyzer::module_implementation_scope::ModuleImplementationScope,
		global_ctx: &GlobalAnalyzerContext,
		clock_graph: &mut crate::analyzer::sensitivity_graph::ClockGraph,
	) -> Result<(), CompilerDiagnosticBuilder> {
		use SignalSensitivity::*;
		log::debug!("Self {:?}", self);
		log::debug!("Other {:?}", rhs);
		match (&self, rhs) {
			(_, NoSensitivity) | (Async(_), _) | (Const(_), Const(_)) | (NoSensitivity, _) => (),
			(Clock(..), Const(_)) => {
				return Err(SemanticError::DifferingSensitivities
					.to_diagnostic_builder()
					.label(location, "Cannot assign a constant signal to a clock signal")
					.label(*rhs.location().unwrap(), "This sensitivity const")
					.label(*self.location().unwrap(), "This is clock sensitivity"));
			},
			(Sync(current, lhs_location), Sync(incoming, _)) => {
				log::debug!("Curent {:?}", current);
				log::debug!("Incoming {:?}", incoming);
				for value in &incoming.list {
					if !current.contains_clock(value.clock_signal) {
						let ids = current.to_ids();
						if clock_graph.is_at_least_one_an_alias(ids, &value.clock_signal) {
							continue;
						}
						let var_rhs = scope.get_variable_by_id(value.clock_signal).unwrap();
						return Err(
							SemanticError::DifferingSensitivities
								.to_diagnostic_builder()
								.label(
									location,
									"Cannot assign signals - sensitivity mismatch. Sensitivty of the destination signal should be a super set of the source signal",
								)
								.label(*lhs_location, format!("This sensitivity list does not contain this clock {:?}", global_ctx.id_table.get_by_key(&var_rhs.var.name).unwrap() ).as_str())
								.label(value.location, format!("This clock {:?} is not present in destination signal sensitivity list", global_ctx.id_table.get_by_key(&var_rhs.var.name).unwrap() ).as_str())
						);
					}
				}
			},
			(Clock(_, Some(current)), Clock(_, Some(incoming))) => {
				clock_graph.insert_clock(incoming, current);
			},
			(Comb(current, lhs_location), Comb(incoming, _)) => {
				log::debug!("Curent {:?}", current);
				log::debug!("Incoming {:?}", incoming);
				for value in &incoming.list {
					if !current.contains_clock(value.clock_signal) {
						let ids = current.to_ids();
						if clock_graph.is_at_least_one_an_alias(ids, &value.clock_signal) {
							continue;
						}
						let var_rhs = scope.get_variable_by_id(value.clock_signal).unwrap();
						return Err(
							SemanticError::DifferingSensitivities
								.to_diagnostic_builder()
								.label(
									location,
									"Cannot assign signals - sensitivity mismatch. Sensitivty of the destination signal should be a super set of the source signal",
								)
								.label(*lhs_location, format!("This sensitivity list does not contain this clock {:?}", global_ctx.id_table.get_by_key(&var_rhs.var.name).unwrap() ).as_str())
								.label(value.location, format!("This clock {:?} is not present in destination signal sensitivity list", global_ctx.id_table.get_by_key(&var_rhs.var.name).unwrap() ).as_str())
						);
					}
				}
			},
			(_, Async(sensitivity_location)) => {
				return Err(SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(*self.location().unwrap(), "This sensitivity is better than asynchronous")
						.label(
							location,
							"Cannot assign signals - sensitivity mismatch. Sensitivty of the destination signal should be a super set of the source signal"
						)
						.label(*sensitivity_location, "This sensitivity is asynchronous")
						,
				);
			},
			(_, Comb(_, sensitivity_location)) => {
				return Err(SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(
							location,
							"Cannot assign signals - sensitivity mismatch. Sensitivty of the destination signal should be a super set of the source signal"
						)
						.label(*sensitivity_location, "This sensitivity is better than comb")
						,
				);
			},
			(Comb(..), _) => (),
			(_, Sync(_, sensitivity_location)) => {
				return Err(SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(
							location,
							"Cannot assign signals - sensitivity mismatch. Sensitivty of the destination signal should be a super set of the source signal"
						)
						.label(*sensitivity_location, "This sensitivity is better than sync")
						,
				);
			},
			(Sync(..), _) => (),
			(Const(sensitivity_location), _) => {
				return Err(SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(
							location,
							"Cannot assign signals - sensitivity mismatch. Sensitivty of the destination signal should be a super set of the source signal"
						)
						.label(*sensitivity_location, "This sensitivity is const")
						.label(*rhs.location().unwrap(), "This sensitivity is worse than const")
						,
				);
			},
			(_, Clock(_, None)) => (),
			_ => unreachable!(),
		}
		Ok(())
	}
	pub fn get_dependencies(&self) -> Vec<InternalVariableId> {
		use SignalSensitivity::*;
		match self {
			Comb(list, _) | Sync(list, _) => list.to_ids(),
			_ => Vec::new(),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::lexer::IdTable;
	use crate::lexer::NumericConstantTable;
	use paste::paste;
	use std::collections::HashMap;
	fn ctx<'a>(id_table: &'a mut IdTable, nc_table: &'a NumericConstantTable) -> GlobalAnalyzerContext<'a> {
		GlobalAnalyzerContext {
			id_table,
			nc_table,
			modules_declared: HashMap::new(),
			generic_modules: HashMap::new(),
			design: hirn::design::DesignHandle::new(),
			diagnostic_buffer: crate::core::DiagnosticBuffer::new(),
		}
	}
	fn span() -> SourceSpan {
		SourceSpan::new_between(0, 0)
	}
	fn async_sensitivity() -> SignalSensitivity {
		SignalSensitivity::Async(span())
	}
	fn clock_sensitivity() -> SignalSensitivity {
		SignalSensitivity::Clock(span(), None)
	}
	fn const_sensitivity() -> SignalSensitivity {
		SignalSensitivity::Const(span())
	}
	fn comb_sensitivity() -> SignalSensitivity {
		SignalSensitivity::Comb(ClockSensitivityList::new(), span())
	}
	fn sync_sensitivity() -> SignalSensitivity {
		SignalSensitivity::Sync(ClockSensitivityList::new(), span())
	}
	macro_rules! sensitivity_test_ok {
		($name1:ident, $name2:ident) => {
			paste! {
				let mut id_table = IdTable::new();
				let nc_table = NumericConstantTable::new();
				assert!([<$name1 _sensitivity>]().can_drive(&[<$name2 _sensitivity>](), span(),&ctx(&mut id_table, &nc_table)).is_ok());
				assert!([<$name2 _sensitivity>]().can_drive(&[<$name1 _sensitivity>](), span(),&ctx(&mut id_table, &nc_table)).is_err());
			}
		};
	}

	#[test]
	fn test_names() {
		assert_eq!(async_sensitivity().name(), "async");
		assert_eq!(clock_sensitivity().name(), "clock");
		assert_eq!(const_sensitivity().name(), "const");
		assert_eq!(comb_sensitivity().name(), "comb");
		assert_eq!(sync_sensitivity().name(), "sync");
	}

	#[test]
	#[should_panic]
	fn test_name_no_sensitivity() {
		SignalSensitivity::NoSensitivity.name();
	}

	#[test]
	fn test_location() {
		assert_eq!(async_sensitivity().location(), Some(&span()));
		assert_eq!(clock_sensitivity().location(), Some(&span()));
		assert_eq!(const_sensitivity().location(), Some(&span()));
		assert_eq!(comb_sensitivity().location(), Some(&span()));
		assert_eq!(sync_sensitivity().location(), Some(&span()));
		assert_eq!(SignalSensitivity::NoSensitivity.location(), None);
	}

	#[test]
	fn is_none() {
		assert!(SignalSensitivity::NoSensitivity.is_none());
		assert!(!async_sensitivity().is_none());
		assert!(!clock_sensitivity().is_none());
		assert!(!const_sensitivity().is_none());
		assert!(!comb_sensitivity().is_none());
		assert!(!sync_sensitivity().is_none());
	}

	#[test]
	fn is_clock() {
		assert!(clock_sensitivity().is_clock());
		assert!(!async_sensitivity().is_clock());
		assert!(!const_sensitivity().is_clock());
		assert!(!comb_sensitivity().is_clock());
		assert!(!sync_sensitivity().is_clock());
		assert!(!SignalSensitivity::NoSensitivity.is_clock());
	}

	#[test]
	fn async_const() {
		sensitivity_test_ok!(async, const);
	}
	#[test]
	fn async_clock() {
		sensitivity_test_ok!(async, clock);
	}
	#[test]
	fn const_clock() {
		sensitivity_test_ok!(clock, const);
	}
}
