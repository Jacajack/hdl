use std::{
	collections::{HashMap, HashSet},
	hash::Hash,
};

use super::{GlobalAnalyzerContext, LocalAnalyzerContext, ModuleImplementationScope, SignalSensitivity};
use bimap::BiHashMap;
use itertools::Itertools;
use petgraph::{
	algo::is_cyclic_directed,
	prelude::{DiGraph, UnGraph},
};

use crate::{
	analyzer::{ModuleInstanceKind, SemanticError, VariableKind},
	core::CompilerDiagnosticBuilder,
	parser::ast::SourceLocation,
	ProvidesCompilerDiagnostic, SourceSpan,
};

use super::module_implementation_scope::InternalVariableId;
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClockGraphEntry {
	id: InternalVariableId,
}

pub struct ClockGraph {
	graph: UnGraph<ClockGraphEntry, ()>,
	graph_entries: BiHashMap<SenstivityGraphIndex, ClockGraphEntry>,
}
impl ClockGraph {
	pub fn new() -> Self {
		Self {
			graph: UnGraph::default(),
			graph_entries: BiHashMap::new(),
		}
	}
	pub fn insert_or_get_index(&mut self, entry: ClockGraphEntry) -> petgraph::stable_graph::NodeIndex {
		match self.graph_entries.get_by_right(&entry) {
			Some(id) => id.index(),
			None => {
				let id: petgraph::stable_graph::NodeIndex = self.graph.add_node(entry);
				self.graph_entries.insert(SenstivityGraphIndex::new(id), entry);
				id
			},
		}
	}
	pub fn get_index(&self, entry: ClockGraphEntry) -> petgraph::stable_graph::NodeIndex {
		self.graph_entries
			.get_by_right(&entry)
			.map(|id| id.index())
			.unwrap()
			.clone()
	}
	pub fn insert_clock(&mut self, from: &InternalVariableId, to: &InternalVariableId) {
		log::debug!("Inserting clock from {:?} to {:?}", from, to);
		let from_id = self.insert_or_get_index(ClockGraphEntry { id: *from });
		let to_id = self.insert_or_get_index(ClockGraphEntry { id: *to });
		self.graph.add_edge(from_id, to_id, ());
		assert!(self.are_clocks_connected(from, to));
	}
	pub fn is_at_least_one_an_alias(&mut self, lhs: Vec<InternalVariableId>, rhs: &InternalVariableId) -> bool {
		log::debug!("Checking if {:?} is an alias of {:?}", lhs, rhs);
		log::debug!("Graph is {:?}", self.graph);
		for lhs in lhs {
			if self.are_clocks_connected(rhs, &lhs) {
				log::debug!("Found alias");
				return true;
			}
		}
		false
	}
	pub fn are_clocks_connected(&mut self, from: &InternalVariableId, to: &InternalVariableId) -> bool {
		let from_id = self.insert_or_get_index(ClockGraphEntry { id: *from });
		let to_id = self.insert_or_get_index(ClockGraphEntry { id: *to });
		petgraph::algo::has_path_connecting(&self.graph, from_id, to_id, None)
	}
}
#[derive(Debug, Clone, Hash, Eq, PartialOrd, Ord)]
pub enum SensitivityGraphEntry {
	Signal(InternalVariableId, SourceSpan),
	Sensitivity(SignalSensitivity),
}
impl PartialEq for SensitivityGraphEntry {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Signal(l0, _), Self::Signal(r0, _)) => l0 == r0,
			(Self::Sensitivity(s1), Self::Sensitivity(s2)) => s1 == s2,
			_ => false,
		}
	}
}
impl SensitivityGraphEntry {
	pub fn location(&self) -> SourceSpan {
		match self {
			Self::Signal(_, location) => *location,
			Self::Sensitivity(sen) => *sen.location().unwrap(),
		}
	}
}
#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
pub struct SenstivityGraphEdge {
	location: SourceSpan,
}
impl SenstivityGraphEdge {
	pub fn new(location: SourceSpan) -> Self {
		Self { location }
	}
	pub fn location(&self) -> SourceSpan {
		self.location
	}
}
impl SensitivityGraphEntry {
	pub fn new_signal(id: InternalVariableId, location: SourceSpan) -> Self {
		Self::Signal(id, location)
	}
	pub fn new_sens(sens: SignalSensitivity) -> Self {
		Self::Sensitivity(sens)
	}
}
#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
pub struct SenstivityGraphIndex {
	index: petgraph::stable_graph::NodeIndex,
}
impl SenstivityGraphIndex {
	pub fn new(index: petgraph::stable_graph::NodeIndex) -> Self {
		Self { index }
	}
	pub fn index(&self) -> petgraph::stable_graph::NodeIndex {
		self.index
	}
}
pub struct SensitivityGraph {
	graph: DiGraph<SensitivityGraphEntry, SenstivityGraphEdge>,
	graph_entries: BiHashMap<SenstivityGraphIndex, SensitivityGraphEntry>,
}

impl SensitivityGraph {
	pub fn add_edges(
		&mut self,
		from: Vec<SensitivityGraphEntry>,
		to: SensitivityGraphEntry,
		edge_loc: SourceSpan,
	) -> Result<(), CompilerDiagnosticBuilder> {
		for node in from {
			self.add_edge(node, to.clone(), SenstivityGraphEdge::new(edge_loc))?;
		}
		Ok(())
	}
	pub fn insert_or_get_index(&mut self, entry: SensitivityGraphEntry) -> petgraph::stable_graph::NodeIndex {
		match self.graph_entries.get_by_right(&entry) {
			Some(id) => id.index(),
			None => {
				let id: petgraph::stable_graph::NodeIndex = self.graph.add_node(entry.clone());
				self.graph_entries.insert(SenstivityGraphIndex::new(id), entry);
				id
			},
		}
	}
	pub fn get_index(&self, entry: &SensitivityGraphEntry) -> petgraph::stable_graph::NodeIndex {
		self.graph_entries
			.get_by_right(entry)
			.map(|id| id.index())
			.unwrap()
			.clone()
	}
	pub fn add_edge(
		&mut self,
		from: SensitivityGraphEntry,
		to: SensitivityGraphEntry,
		edge: SenstivityGraphEdge,
	) -> Result<(), CompilerDiagnosticBuilder> {
		assert!(!is_cyclic_directed(&self.graph));

		let from_id = self.insert_or_get_index(from);
		let to_id = self.insert_or_get_index(to);
		self.graph.add_edge(from_id, to_id, edge);

		log::debug!("Graph: {:?}", self.graph);

		if is_cyclic_directed(&self.graph) {
			return Err(SemanticError::CyclicDependency
				.to_diagnostic_builder()
				.label(edge.location(), "This edge creates a cyclic dependency"));
		}
		Ok(())
	}
	pub fn new() -> Self {
		Self {
			graph: DiGraph::new(),
			graph_entries: BiHashMap::new(),
		}
	}
	pub fn get_node_sensitivity_only_clock(
		&self,
		node: SensitivityGraphEntry,
		ctx: &mut ModuleImplementationScope,
		global_ctx: &GlobalAnalyzerContext,
		already_visited: &mut HashSet<SensitivityGraphEntry>,
		sensitivty_nodes: &mut HashMap<SignalSensitivity, SensitivityGraphEntry>,
		clock_graph: &mut ClockGraph,
	) -> miette::Result<()> {
		log::debug!(
			"Getting sensitivity for node {:?} with already visited {:?}",
			node,
			already_visited
		);
		match node {
			SensitivityGraphEntry::Signal(id, _) => {
				if already_visited.contains(&node) {
					return Ok(());
				}
				let var_sens = ctx.get_variable_by_id(id).unwrap();
				match var_sens.var.kind {
					super::VariableKind::Signal(sig) => {
						if let SignalSensitivity::Clock(..) = sig.sensitivity {
							log::debug!("Getting sensitivity for clock {:?}", sig.sensitivity);
							self.get_node_sensitivity(
								node,
								ctx,
								global_ctx,
								already_visited,
								sensitivty_nodes,
								clock_graph,
							)?;
						}
					},
					super::VariableKind::Generic(_) => (),
					_ => unreachable!(),
				};
				Ok(())
			},
			SensitivityGraphEntry::Sensitivity(_) => Ok(()),
		}
	}
	pub fn get_node_sensitivity(
		&self,
		node: SensitivityGraphEntry,
		ctx: &mut ModuleImplementationScope,
		global_ctx: &GlobalAnalyzerContext,
		already_visited: &mut HashSet<SensitivityGraphEntry>,
		sensitivty_nodes: &mut HashMap<SignalSensitivity, SensitivityGraphEntry>,
		clock_graph: &mut ClockGraph,
	) -> miette::Result<SignalSensitivity> {
		log::debug!(
			"Getting sensitivity for node {:?} with already visited {:?}",
			node,
			already_visited
		);
		match node {
			SensitivityGraphEntry::Signal(id, location) => {
				if already_visited.contains(&node) {
					return Ok(ctx.get_variable_by_id(id).unwrap().get_sensitivity());
				}
				let neighbours = self
					.graph
					.neighbors_directed(self.get_index(&node), petgraph::Direction::Incoming);
				let mut sens = SignalSensitivity::NoSensitivity;
				let mut var_sens = ctx.get_variable_by_id(id).unwrap();

				let return_sig = match &mut var_sens.var.kind {
					super::VariableKind::Signal(sig) => {
						for neighbour in neighbours {
							sens.evaluate_sensitivity(
								vec![self.get_node_sensitivity(
									self.graph_entries
										.get_by_left(&SenstivityGraphIndex::new(neighbour))
										.unwrap()
										.clone(),
									ctx,
									global_ctx,
									already_visited,
									sensitivty_nodes,
									clock_graph,
								)?],
								location,
							);
							sig.sensitivity
								.can_drive_report_as_builder(
									&sens,
									self.graph
										.edges_connecting(neighbour, self.get_index(&node))
										.collect_vec()
										.first()
										.unwrap()
										.weight()
										.location,
									ctx,
									global_ctx,
									clock_graph,
								)
								.map_err(|mut err| {
									log::debug!("Sensitivity nodes are {:?}", sensitivty_nodes);
									log::debug!("Indexes are {:?}", self.graph_entries);
									let origin_node = match sensitivty_nodes.get(&sens) {
										Some(node) => self.get_index(node),
										None => neighbour,
									};
									log::debug!("Origin node is {:?}", origin_node);
									log::debug!("Graph is {:?}", self.graph);
									let nodes_between = petgraph::algo::all_simple_paths::<Vec<_>, _>(
										&self.graph,
										origin_node,
										self.get_index(&node),
										0,
										None,
									)
									.collect_vec()
									.first()
									.unwrap()
									.clone();
									log::debug!("Nodes between are {:?}", nodes_between);

									let mut first = origin_node.clone();
									for i in 1..nodes_between.len() - 1 {
										let second = nodes_between[i];
										let edge = self
											.graph
											.edges_connecting(first, second)
											.collect_vec()
											.first()
											.unwrap()
											.weight();
										err = err.label(edge.location(), "This edge is part of the sensitivity path");
										first = second;
									}
									err.build()
								})?;
						}
						if sig.sensitivity == SignalSensitivity::NoSensitivity {
							sig.sensitivity = sens.clone();
						}
						already_visited.insert(node.clone());
						sig.sensitivity.clone()
					},
					super::VariableKind::Generic(_) => SignalSensitivity::Const(var_sens.var.location),
					_ => unreachable!(),
				};
				log::debug!("Inserting {:?} into sensitivity nodes", node);
				log::debug!("Sensitivity for all predecessor of {:?} is {:?}", node, sens);
				if !sensitivty_nodes.contains_key(&return_sig) {
					sensitivty_nodes.insert(return_sig.clone(), node);
				}
				ctx.redeclare_variable(var_sens.clone());
				Ok(return_sig)
			},
			SensitivityGraphEntry::Sensitivity(sens) => Ok(sens),
		}
	}
	pub fn verify(
		&self,
		scope: &mut super::ModuleImplementationScope,
		global_ctx: &GlobalAnalyzerContext,
	) -> miette::Result<()> {
		assert!(!is_cyclic_directed(&self.graph));
		let mut visited = HashSet::new();
		let mut sens_nodes = HashMap::new();
		let mut clock_graph = ClockGraph::new();
		for node in self.graph.raw_nodes() {
			self.get_node_sensitivity_only_clock(
				node.weight.clone(),
				scope,
				global_ctx,
				&mut visited,
				&mut sens_nodes,
				&mut clock_graph,
			)?;
		}
		for node in self.graph.raw_nodes() {
			self.get_node_sensitivity(
				node.weight.clone(),
				scope,
				global_ctx,
				&mut visited,
				&mut sens_nodes,
				&mut clock_graph,
			)?;
		}
		Ok(())
	}
}

impl crate::parser::ast::Expression {
	// only on lhs
	pub fn get_internal_id(
		&self,
		ctx: &ModuleImplementationScope,
		scope_id: usize,
	) -> (InternalVariableId, SourceSpan) {
		use crate::parser::ast::Expression::*;
		match self {
			Identifier(id) => {
				let var = ctx.get_variable(scope_id, &id.id).unwrap();
				(var.id, var.var.location)
			},
			ParenthesizedExpression(expr) => expr.expression.get_internal_id(ctx, scope_id),
			PostfixWithIndex(expr) => expr.expression.get_internal_id(ctx, scope_id),
			PostfixWithRange(expr) => expr.expression.get_internal_id(ctx, scope_id),
			PostfixWithId(expr) => todo!(),
			_ => unreachable!(),
		}
	}
	pub fn get_sensitivity_entry(
		&self,
		global_ctx: &GlobalAnalyzerContext,
		ctx: &LocalAnalyzerContext,
		scope_id: usize,
	) -> Vec<SensitivityGraphEntry> {
		use crate::parser::ast::Expression::*;
		match self {
			Number(_) => vec![SensitivityGraphEntry::new_sens(SignalSensitivity::Const(
				self.get_location(),
			))],
			Identifier(id) => {
				let var = ctx.scope.get_variable(scope_id, &id.id).unwrap();
				vec![SensitivityGraphEntry::new_signal(var.id, var.var.location)]
			},
			ParenthesizedExpression(expr) => expr.expression.get_sensitivity_entry(global_ctx, ctx, scope_id),
			MatchExpression(m) => {
				let mut vec = vec![m.value.get_sensitivity_entry(global_ctx, ctx, scope_id)];
				for arm in &m.statements {
					use crate::parser::ast::MatchExpressionAntecendent::*;
					match &arm.antecedent {
						Expression { expressions, .. } => {
							for expr in expressions {
								vec.push(expr.get_sensitivity_entry(global_ctx, ctx, scope_id));
							}
						},
						Default { .. } => (),
					}
					vec.push(arm.expression.get_sensitivity_entry(global_ctx, ctx, scope_id));
				}
				vec.into_iter().flatten().collect()
			},
			ConditionalExpression(cond) => {
				let mut vec = Vec::new();
				for arm in &cond.statements {
					use crate::parser::ast::MatchExpressionAntecendent::*;
					match &arm.antecedent {
						Expression { expressions, .. } => {
							for expr in expressions {
								vec.push(expr.get_sensitivity_entry(global_ctx, ctx, scope_id));
							}
						},
						Default { .. } => (),
					}
					vec.push(arm.expression.get_sensitivity_entry(global_ctx, ctx, scope_id));
				}
				vec.into_iter().flatten().collect()
			},
			Tuple(_) => unreachable!(),
			TernaryExpression(tern) => vec![
				tern.condition.get_sensitivity_entry(global_ctx, ctx, scope_id),
				tern.true_branch.get_sensitivity_entry(global_ctx, ctx, scope_id),
				tern.false_branch.get_sensitivity_entry(global_ctx, ctx, scope_id),
			]
			.into_iter()
			.flatten()
			.collect(),
			PostfixWithIndex(index) => index.expression.get_sensitivity_entry(global_ctx, ctx, scope_id),
			PostfixWithRange(range) => range.expression.get_sensitivity_entry(global_ctx, ctx, scope_id),
			PostfixWithArgs(function) => {
				let func_name = global_ctx.id_table.get_value(&function.id);
				match func_name.as_str() {
					"zeroes" | "ones" => vec![SensitivityGraphEntry::new_sens(SignalSensitivity::Const(
						self.get_location(),
					))],
					"rep" | "trunc" | "zext" | "ext" | "sext" | "fold_or" | "fold_xor" | "fold_and" => function
						.argument_list
						.first()
						.unwrap()
						.get_sensitivity_entry(global_ctx, ctx, scope_id),
					"join" => function
						.argument_list
						.iter()
						.map(|arg| arg.get_sensitivity_entry(global_ctx, ctx, scope_id))
						.flatten()
						.collect(),
					_ => unreachable!(),
				}
			},
			PostfixWithId(postfix) => {
				let var = ctx.scope.get_variable(scope_id, &postfix.expression).unwrap();
				if let VariableKind::ModuleInstance(inst) = &var.var.kind {
					use ModuleInstanceKind::*;
					match &inst.kind {
						Module(m) => {
							let v = m.interface.get(&postfix.id).unwrap();
							let var = ctx.scope.get_variable_by_id(*v).unwrap();
							return vec![SensitivityGraphEntry::new_signal(var.id, var.var.location)];
						},
						Register(reg) => {
							return match global_ctx.id_table.get_value(&postfix.id).as_str() {
								"clk" => vec![SensitivityGraphEntry::new_signal(reg.clk, reg.location)],
								"next" => vec![SensitivityGraphEntry::new_signal(reg.next, reg.location)],
								"nreset" => vec![SensitivityGraphEntry::new_signal(reg.nreset, reg.location)],
								"data" => vec![SensitivityGraphEntry::new_signal(reg.data, reg.location)],
								"en" => vec![SensitivityGraphEntry::new_signal(reg.enable, reg.location)],
								_ => unreachable!(),
							};
						},
					}
				}
				unreachable!()
			},
			UnaryOperatorExpression(unary) => unary.expression.get_sensitivity_entry(global_ctx, ctx, scope_id),
			UnaryCastExpression(cast) => {
				let c = ctx.casts.get(&self.get_location()).unwrap();
				if c.dest_sensitivity.is_none() {
					return cast.expression.get_sensitivity_entry(global_ctx, ctx, scope_id);
				}
				else {
					return vec![SensitivityGraphEntry::new_sens(c.dest_sensitivity.clone())];
				}
			},
			BinaryExpression(binop) => vec![
				binop.lhs.get_sensitivity_entry(global_ctx, ctx, scope_id),
				binop.rhs.get_sensitivity_entry(global_ctx, ctx, scope_id),
			]
			.into_iter()
			.flatten()
			.collect(),
		}
	}
}
