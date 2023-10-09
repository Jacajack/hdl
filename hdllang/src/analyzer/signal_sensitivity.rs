use crate::{analyzer::SemanticError, lexer::IdTableKey, ProvidesCompilerDiagnostic, SourceSpan};

use super::GlobalAnalyzerContext;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct EdgeSensitivity {
	pub clock_signal: IdTableKey,
	pub on_rising: bool,
	pub location: SourceSpan,
}

/// Determines sensitivity of a signal to certain clocks
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClockSensitivityList {
	pub list: Vec<EdgeSensitivity>,
}
impl ClockSensitivityList {
	pub fn contains_clock(&self, id: IdTableKey) -> bool {
		for edge in &self.list {
			if edge.clock_signal == id {
				return true;
			}
		}
		false
	}
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SignalSensitivity {
	Async(SourceSpan),
	Comb(ClockSensitivityList, SourceSpan),
	Sync(ClockSensitivityList, SourceSpan),
	Clock(SourceSpan),
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
			Clock(_) => "clock",
			Const(_) => "const",
			NoSensitivity => "none",
		}
	}
	pub fn location(&self) -> Option<&SourceSpan> {
		use SignalSensitivity::*;
		match self {
			Async(x) => Some(x),
			Comb(_, x) => Some(x),
			Sync(_, x) => Some(x),
			Clock(x) => Some(x),
			Const(x) => Some(x),
			NoSensitivity => None,
		}
	}
	pub fn can_drive(
		&mut self,
		rhs: &SignalSensitivity,
		location: SourceSpan,
		global_ctx: &GlobalAnalyzerContext,
	) -> miette::Result<()> {
		use SignalSensitivity::*;
		match (&self, rhs) {
			(_, NoSensitivity)
			| (Async(_), Async(_))
			| (Sync(..), Sync(..))
			| (Const(_), Const(_))
			| (Clock(_), Clock(_)) => (),
			(NoSensitivity, _) => *self = rhs.clone(),
			(Comb(curent, lhs_location), Comb(incoming, incoming_location)) => {
				for value in &incoming.list {
					if !curent.contains_clock(value.clock_signal) {
						return Err(miette::Report::new(
							SemanticError::DifferingSensitivities
								.to_diagnostic_builder()
								.label(
									location,
									"Cannot assign signals - sensitivity mismatch. Sensitivty of the land hand side should be a super set of the right hand side",
								)
								.label(*lhs_location, format!("This sensitivity list does not contain this clock {:?}", global_ctx.id_table.get_by_key(&value.clock_signal).unwrap() ).as_str())
								.label(value.location, format!("This clock {:?} is not present in left hand side sensitivity list", global_ctx.id_table.get_by_key(&value.clock_signal).unwrap() ).as_str())
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
			(Const(_), _) => (),
			(_, Const(sensitivity_location)) => {
				return Err(miette::Report::new(
					SemanticError::DifferingSensitivities
						.to_diagnostic_builder()
						.label(
							location,
							"Cannot assign signals - sensitivity mismatch. Sensitivity of the land hand side should be worse or the same as the right hand side",
						)
						.label(*sensitivity_location, "This sensitivity is const")
						.build(),
				));
			},
		}
		Ok(())
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::lexer::NumericConstantTable;
	use crate::{core::NumericConstant, lexer::IdTable};
	use paste::paste;
	use std::collections::HashMap;
	use std::hash::Hash;
	fn ctx<'a>(id_table: &'a IdTable, nc_table: &'a NumericConstantTable) -> GlobalAnalyzerContext<'a> {
		GlobalAnalyzerContext {
			id_table,
			nc_table,
			modules_declared: HashMap::new(),
			generic_modules: HashMap::new(),
			design: hirn::design::Design::new(),
		}
	}
	fn span() -> SourceSpan {
		SourceSpan::new_between(0, 0)
	}
	fn async_sensitivity() -> SignalSensitivity {
		SignalSensitivity::Async(span())
	}
	fn clock_sensitivity() -> SignalSensitivity {
		SignalSensitivity::Clock(span())
	}
	fn const_sensitivity() -> SignalSensitivity {
		SignalSensitivity::Const(span())
	}
	macro_rules! sensitivity_test_ok {
		($name1:ident, $name2:ident) => {
			paste! {
				let id_table = IdTable::new();
				let nc_table = NumericConstantTable::new();
				assert!([<$name1 _sensitivity>]().can_drive(&[<$name2 _sensitivity>](), span(),&ctx(&id_table, &nc_table)).is_ok());
				assert!([<$name2 _sensitivity>]().can_drive(&[<$name1 _sensitivity>](), span(),&ctx(&id_table, &nc_table)).is_err());
			}
		};
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
		sensitivity_test_ok!(const, clock);
	}
}
