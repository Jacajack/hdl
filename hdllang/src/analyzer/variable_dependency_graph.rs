use bimap::BiHashMap;
use petgraph::prelude::DiGraph;

use crate::core::CompilerDiagnosticBuilder;

use super::module_implementation_scope::InternalVariableId;


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
impl DependencyGraph{
	pub fn new() -> Self {
		Self {
			graph: DiGraph::new(),
			graph_entries: BiHashMap::new(),
		}
	}
	pub fn insert_or_get_index(&mut self, entry: DependencyGraphEntry) -> petgraph::stable_graph::NodeIndex {
		match self.graph_entries.get_by_right(&entry) {
			Some(id) => id.index(),
			None => {
				let id: petgraph::stable_graph::NodeIndex = self.graph.add_node(entry.clone());
				self.graph_entries.insert(DependencyGraphIndex::new(id), entry);
				id
			},
		}
	}
	pub fn add_node(&mut self, entry: InternalVariableId) {
		let entry = DependencyGraphEntry::new(entry);
		self.insert_or_get_index(entry);
	}
	pub fn add_edge(
		&mut self,
		from: DependencyGraphEntry,
		to: DependencyGraphEntry,
	) -> Result<(), CompilerDiagnosticBuilder> {
		let from_id = self.insert_or_get_index(from);
		let to_id = self.insert_or_get_index(to);

		if from_id != to_id {
			self.graph.add_edge(from_id, to_id, ());
		}

		log::debug!("Graph: {:?}", self.graph);
		Ok(())
	}
}
