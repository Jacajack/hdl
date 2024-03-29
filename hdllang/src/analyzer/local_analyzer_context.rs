use crate::analyzer::GlobalAnalyzerContext;
use crate::analyzer::{BusWidth, ModuleImplementationScope};
/// Per module context for semantic analysis
use crate::{core::*, parser::ast::*, SourceSpan};
use std::collections::HashMap;

use super::module_implementation_scope::ExpressionEntryId;
#[derive(Clone)]
pub struct LocalAnalyzerContext {
	pub scope: ModuleImplementationScope,
	pub nc_widths: HashMap<SourceSpan, NumericConstant>,
	pub ncs_to_be_exted: HashMap<SourceSpan, ExpressionEntryId>,
	pub array_or_bus: HashMap<SourceSpan, bool>, // to distinguish between array and bus in index expr
	pub widths_map: HashMap<SourceSpan, BusWidth>,
	pub scope_map: HashMap<SourceSpan, usize>,
	module_id: IdTableKey,
	pub sensitivity_graph: super::SensitivityGraph,
	are_we_in_true_branch: Vec<bool>,
	pub number_of_recursive_calls: usize,
	pub casts: HashMap<SourceSpan, Cast>,
	pub depenency_graph: super::DependencyGraph,
}
impl LocalAnalyzerContext {
	pub fn new(module_id: IdTableKey, scope: ModuleImplementationScope) -> Box<Self> {
		Box::new(LocalAnalyzerContext {
			scope,
			scope_map: HashMap::new(),
			module_id,
			nc_widths: HashMap::new(),
			ncs_to_be_exted: HashMap::new(),
			widths_map: HashMap::new(),
			sensitivity_graph: super::SensitivityGraph::new(),
			casts: HashMap::new(),
			are_we_in_true_branch: vec![true], // initial value is true :)
			number_of_recursive_calls: 0,
			array_or_bus: HashMap::new(),
			depenency_graph: super::DependencyGraph::new(),
		})
	}
	pub fn module_id(&self) -> IdTableKey {
		self.module_id
	}
	pub fn add_branch(&mut self, branch: bool) {
		log::debug!("Adding branch: {}", branch);
		self.are_we_in_true_branch.push(branch);
	}
	pub fn pop_branch(&mut self) {
		self.are_we_in_true_branch.pop();
	}
	pub fn are_we_in_true_branch(&self) -> bool {
		for branch in self.are_we_in_true_branch.iter().rev() {
			if !*branch {
				log::debug!("We are not in true branch");
				return false;
			}
		}
		log::debug!("We are in true branch");
		true
	}
	pub fn always_true_branch(&self) -> bool {
		self.are_we_in_true_branch.len() == 1
	}
	pub fn second_pass(&mut self, ctx: &mut GlobalAnalyzerContext) -> miette::Result<()> {
		log::debug!("Second pass");
		self.sensitivity_graph.verify(&mut self.scope, ctx)?;
		self.scope.second_pass(ctx, &mut self.depenency_graph)?;
		log::debug!("Dependency graph: {:?}", self.depenency_graph);
		Ok(())
	}
}
/// To be deleted and replaced by LocalAnalyzerContext
pub struct AdditionalContext {
	pub nc_widths: HashMap<SourceSpan, NumericConstant>,
	pub ncs_to_be_exted: HashMap<SourceSpan, ExpressionEntryId>,
	pub array_or_bus: HashMap<SourceSpan, bool>, // to distinguish between array and bus in index expr
	pub casts: HashMap<SourceSpan, Cast>,
}
impl AdditionalContext {
	pub fn new(
		nc_widths: HashMap<SourceSpan, NumericConstant>,
		ncs_to_be_exted: HashMap<SourceSpan, ExpressionEntryId>,
		array_or_bus: HashMap<SourceSpan, bool>,
		casts: HashMap<SourceSpan, Cast>,
	) -> Self {
		AdditionalContext {
			nc_widths,
			ncs_to_be_exted,
			array_or_bus,
			casts,
		}
	}
}
