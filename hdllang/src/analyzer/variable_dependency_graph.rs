use bimap::BiHashMap;
use hirn::design::ScopeHandle;
use petgraph::prelude::DiGraph;

use crate::core::comment_table;

use super::{
	module_implementation_scope::{InternalVariableId, ModuleImplementationScope},
	AdditionalContext,
};

#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct DependencyGraphEntry {
	id: InternalVariableId,
}
impl DependencyGraphEntry {
	pub fn new(id: InternalVariableId) -> Self {
		Self { id }
	}
	pub fn id(&self) -> InternalVariableId {
		self.id
	}
}
#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
pub struct DependencyGraphIndex {
	index: petgraph::stable_graph::NodeIndex,
}
impl DependencyGraphIndex {
	pub fn new(index: petgraph::stable_graph::NodeIndex) -> Self {
		Self { index }
	}
	pub fn index(&self) -> petgraph::stable_graph::NodeIndex {
		self.index
	}
}
#[derive(Debug, Clone)]
pub struct DependencyGraph {
	graph: DiGraph<DependencyGraphEntry, ()>,
	graph_entries: BiHashMap<DependencyGraphIndex, DependencyGraphEntry>,
}
impl DependencyGraph {
	pub fn new() -> Self {
		Self {
			graph: DiGraph::new(),
			graph_entries: BiHashMap::new(),
		}
	}
	pub fn insert_or_get_index(&mut self, entry: DependencyGraphEntry) -> petgraph::stable_graph::NodeIndex {
		log::debug!("Inserting entry {:?} into graph", entry);
		match self.graph_entries.get_by_right(&entry) {
			Some(id) => id.index(),
			None => {
				let id: petgraph::stable_graph::NodeIndex = self.graph.add_node(entry.clone());
				self.graph_entries.insert(DependencyGraphIndex::new(id), entry);
				id
			},
		}
	}
	pub fn get_index(&self, entry: DependencyGraphEntry) -> petgraph::stable_graph::NodeIndex {
		self.graph_entries
			.get_by_right(&entry)
			.map(|id| id.index())
			.unwrap_or_else(|| panic!("Entry {:?} not found in graph", entry))
	}
	pub fn add_node(&mut self, entry: InternalVariableId) {
		let entry = DependencyGraphEntry::new(entry);
		self.insert_or_get_index(entry);
	}
	fn add_edge(&mut self, from: DependencyGraphEntry, to: DependencyGraphEntry) {
		let from_id = self.insert_or_get_index(from);
		let to_id = self.insert_or_get_index(to);

		if from_id != to_id {
			self.graph.add_edge(from_id, to_id, ());
		}

		assert!(!petgraph::algo::is_cyclic_directed(&self.graph));
		log::debug!("Graph: {:?}", self.graph);
	}
	pub fn add_edges(&mut self, from: Vec<InternalVariableId>, to: InternalVariableId) {
		let to = DependencyGraphEntry::new(to);
		for from in from {
			let from = DependencyGraphEntry::new(from);
			self.add_edge(from, to);
		}
	}
	pub fn register(
		&self,
		nc_table: &crate::core::NumericConstantTable,
		id_table: &crate::core::IdTable,
		comment_table: &comment_table::CommentTable,
		additional_ctx: Option<&AdditionalContext>,
		scope: &mut ModuleImplementationScope,
		scope_id: usize,
		scope_handle: &mut ScopeHandle,
		id: InternalVariableId,
	) {
		if scope.is_already_registered(id) {
			return;
		}
		let var = scope.get_variable_by_id(id).expect("Variable not found");
		if var.var.kind.is_module_instance() || var.is_iterated {
			return;
		}
		log::debug!("Registering variable {}", id_table.get_by_key(&var.var.name).unwrap());
		let entry = DependencyGraphEntry::new(id);
		let index = self.get_index(entry);
		let mut neigh = self
			.graph
			.neighbors_directed(index, petgraph::Direction::Incoming)
			.detach();
		while let Some(dep) = neigh.next(&self.graph) {
			let dep = self
				.graph_entries
				.get_by_left(&DependencyGraphIndex::new(dep.1))
				.unwrap();
			if scope.is_already_registered(dep.id()) {
				continue;
			}
			else {
				self.register(
					nc_table,
					id_table,
					comment_table,
					additional_ctx,
					scope,
					scope_id,
					scope_handle,
					dep.id(),
				);
			}
		}
		if scope.is_already_registered(id) {
			return;
		}
		let var = scope.get_variable_by_id(id).expect("Variable not found");
		let builder = scope_handle.new_signal(&id_table.get_value(&var.var.name)).unwrap();
		let api_id = var
			.var
			.register(nc_table, id_table, comment_table, scope_id, scope, additional_ctx, builder)
			.unwrap();
		scope.insert_api_id(id, api_id);
		//scope.register_variable(id);
	}
}
