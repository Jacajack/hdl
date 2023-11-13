use crate::analyzer::GlobalAnalyzerContext;
use crate::analyzer::{BusWidth, ModuleImplementationScope};
/// Per module context for semantic analysis
use crate::{core::*, parser::ast::*, SourceSpan};
use std::collections::HashMap;
pub struct LocalAnalyzerContext {
	pub scope: ModuleImplementationScope,
	pub nc_widths: HashMap<SourceSpan, NumericConstant>,
	pub array_or_bus: HashMap<SourceSpan, bool>, // to distinguish between array and bus in index expr
	pub widths_map: HashMap<SourceSpan, BusWidth>,
	pub scope_map: HashMap<SourceSpan, usize>,
	pub module_id: IdTableKey,
	pub sensitivity_graph: super::SensitivityGraph,
	are_we_in_true_branch: Vec<bool>,
	pub number_of_recursive_calls: usize,
	pub casts: HashMap<SourceSpan, Cast>,
}
impl LocalAnalyzerContext {
	pub fn new(module_id: IdTableKey, scope: ModuleImplementationScope) -> Self {
		LocalAnalyzerContext {
			scope,
			scope_map: HashMap::new(),
			module_id,
			nc_widths: HashMap::new(),
			widths_map: HashMap::new(),
			sensitivity_graph: super::SensitivityGraph::new(),
			casts: HashMap::new(),
			are_we_in_true_branch: vec![true], // initial value is true :)
			number_of_recursive_calls: 0,
			array_or_bus: HashMap::new(),
		}
	}
	pub fn add_branch(&mut self, branch: bool) {
		self.are_we_in_true_branch.push(branch);
	}
	pub fn pop_branch(&mut self) {
		self.are_we_in_true_branch.pop();
	}
	pub fn are_we_in_true_branch(&self) -> bool {
		for branch in self.are_we_in_true_branch.iter().rev() {
			if !*branch {
				return false;
			}
		}
		true
	}
	pub fn always_true_branch(&self) -> bool {
		self.are_we_in_true_branch.len() == 1
	}
	pub fn second_pass(&mut self, ctx: &mut GlobalAnalyzerContext) -> miette::Result<()> {
		log::debug!("Second pass");
		self.sensitivity_graph.verify(&mut self.scope, ctx)?;
		self.scope.second_pass(ctx)?;
		Ok(())
	}
}
/// To be deleted and replaced by LocalAnalyzerContext
pub struct AdditionalContext {
	pub nc_widths: HashMap<SourceSpan, NumericConstant>,
	pub array_or_bus: HashMap<SourceSpan, bool>, // to distinguish between array and bus in index expr
	pub casts: HashMap<SourceSpan, Cast>,
}
impl AdditionalContext {
	pub fn new(
		nc_widths: HashMap<SourceSpan, NumericConstant>,
		array_or_bus: HashMap<SourceSpan, bool>,
		casts: HashMap<SourceSpan, Cast>,
	) -> Self {
		AdditionalContext {
			nc_widths,
			array_or_bus,
			casts,
		}
	}
}
