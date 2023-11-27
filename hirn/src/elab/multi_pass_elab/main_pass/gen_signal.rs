use std::sync::Arc;

use log::{debug, error};

use crate::{
	design::{Evaluates, HasSensitivity, ModuleHandle, ScopeHandle, SignalId, SignalSlice},
	elab::{ElabAssumptionsBase, ElabMessageKind, ElabSignal},
};

use super::{MainPassCtx, ScopePassId};

/// Signal ID coupled with scope pass ID to distingush between generated signals
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedSignalId {
	id: SignalId,
	pass_id: Option<ScopePassId>,
	instance_id: Option<usize>,
}

impl GeneratedSignalId {
	pub fn signal(&self) -> SignalId {
		self.id
	}

	pub fn pass_id(&self) -> Option<ScopePassId> {
		self.pass_id
	}

	pub fn instance_id(&self) -> Option<usize> {
		self.instance_id
	}

	pub fn is_external(&self) -> bool {
		self.instance_id.is_some()
	}
}

/// References specific field of a generated signal
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedSignalRef {
	id: GeneratedSignalId,
	index: Option<u32>,
}

impl GeneratedSignalRef {
	pub fn new(id: GeneratedSignalId, index: Option<u32>) -> Self {
		Self { id, index }
	}

	pub fn gen_id(&self) -> GeneratedSignalId {
		self.id
	}

	pub fn signal(&self) -> SignalId {
		self.id.signal()
	}

	pub fn pass_id(&self) -> Option<ScopePassId> {
		self.id.pass_id()
	}

	pub fn index(&self) -> Option<u32> {
		self.index
	}

	pub fn is_external(&self) -> bool {
		self.id.is_external()
	}
}

/// Represents a generated signal (width + dimensions evaluated)
pub struct GeneratedSignal {
	width: u32,
	dimensions: Vec<usize>,
	total_fields: usize,
	instance_id: Option<usize>,
}

impl GeneratedSignal {
	pub fn width(&self) -> u32 {
		self.width
	}

	pub fn total_fields(&self) -> usize {
		self.total_fields
	}

	pub fn is_array(&self) -> bool {
		self.dimensions.len() > 0
	}

	pub fn dimensions(&self) -> &[usize] {
		&self.dimensions
	}

	pub fn instance_id(&self) -> Option<usize> {
		self.instance_id
	}

	pub fn is_external(&self) -> bool {
		self.instance_id.is_some()
	}
}

impl MainPassCtx {
	pub fn get_generated_signal(&self, id: &GeneratedSignalId) -> &GeneratedSignal {
		self.signals.get(&id).expect("Generated signal not registered")
	}

	/// Returns a generated signal ID based on design signal ID and scope pass ID
	pub fn get_generated_signal_id(&self, id: SignalId) -> GeneratedSignalId {
		let sig = self.design.get_signal(id).expect("signal not in design");
		GeneratedSignalId {
			id,
			pass_id: Some(self.get_scope_pass_id(sig.parent_scope)),
			instance_id: None,
		}
	}

	pub fn get_ext_generated_signal_id(&self, id: SignalId, instance_id: usize) -> GeneratedSignalId {
		GeneratedSignalId {
			id,
			pass_id: None,
			instance_id: Some(instance_id),
		}
	}

	// pub fn get_int_ext_generated_signal_id(
	// 	&self,
	// 	id: SignalId,
	// 	instance_id: Option<usize>,
	// ) -> GeneratedSignalId {
	// 	if let Some(instance_id) = instance_id {
	// 		self.get_ext_generated_signal_id(id, instance_id)
	// 	}
	// 	else {
	// 		self.get_generated_signal_id(id)
	// 	}
	// }

	fn get_generated_signal_ref_impl(
		&self,
		slice: &SignalSlice,
		gen_id: GeneratedSignalId,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<GeneratedSignalRef, ElabMessageKind> {
		if slice.indices.is_empty() {
			Ok(GeneratedSignalRef {
				id: gen_id,
				index: None,
			})
		}
		else {
			let gen_sig = self.get_generated_signal(&gen_id);
			assert_eq!(
				slice.indices.len(),
				gen_sig.dimensions.len(),
				"Slice/signal rank mismatch"
			);

			// Eval indices
			let index_vals: Result<Vec<_>, _> = slice
				.indices
				.iter()
				.map(|expr| expr.eval(&assumptions)?.try_into_i64().into())
				.collect();
			let index_vals = index_vals?;

			// Check for invalid array indices
			for (index, size) in index_vals.iter().zip(gen_sig.dimensions.iter()) {
				if *index < 0 || *index >= (*size as i64) {
					error!("Signal {:?} is accessed with invalid index {}", slice.signal, index);
					return Err(ElabMessageKind::InvalidArrayIndex);
				}
			}

			let gen_sig = self.signals.get(&gen_id).expect("Generated signal not registered");
			let index = index_vals
				.iter()
				.zip(gen_sig.dimensions.iter())
				.fold(0, |acc, (index, size)| -> u32 {
					acc * (*size as u32) + (*index as u32)
				});

			Ok(GeneratedSignalRef {
				id: gen_id,
				index: Some(index),
			})
		}
	}

	pub fn get_generated_signal_ref(
		&self,
		slice: &SignalSlice,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<GeneratedSignalRef, ElabMessageKind> {
		let gen_id = self.get_generated_signal_id(slice.signal);
		self.get_generated_signal_ref_impl(slice, gen_id, assumptions)
	}

	pub fn get_ext_generated_signal_ref(
		&self,
		slice: &SignalSlice,
		instance_id: usize,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<GeneratedSignalRef, ElabMessageKind> {
		let gen_id = self.get_ext_generated_signal_id(slice.signal, instance_id);
		self.get_generated_signal_ref_impl(slice, gen_id, assumptions)
	}

	pub fn get_int_ext_generated_signal_ref(
		&self,
		slice: &SignalSlice,
		instance_id: Option<usize>,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<GeneratedSignalRef, ElabMessageKind> {
		if let Some(instance_id) = instance_id {
			self.get_ext_generated_signal_ref(slice, instance_id, assumptions)
		}
		else {
			self.get_generated_signal_ref(slice, assumptions)
		}
	}

	fn declare_signal_impl(
		&mut self,
		id: SignalId,
		validation_scope: ScopeHandle,
		pass_id: Option<ScopePassId>,
		ext_instance_id: Option<usize>,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<GeneratedSignalId, ElabMessageKind> {
		let sig = self.design.get_signal(id).expect("signal not in design");
		// let pass_id = self.get_scope_pass_id(sig.parent_scope);
		// let scope_handle = self.design.get_scope_handle(sig.parent_scope).unwrap();

		assert_ne!(pass_id.is_some(), ext_instance_id.is_some());
		assert!(!sig.is_generic());

		debug!(
			"Signal '{}' declared in pass {:?} (instance: {:?}",
			sig.name(),
			pass_id,
			ext_instance_id
		);

		// Check signal rank
		if sig.rank() > self.config.max_array_rank {
			error!("Signal {} has rank {} which is out of range", sig.name(), sig.rank());
			return Err(ElabMessageKind::InvalidArrayRank(sig.rank() as u32));
		}

		// Evaluate width & validate range
		sig.width().validate(&assumptions.clone(), &validation_scope)?;
		let width = sig.width().eval(&assumptions)?.try_into_i64()?;
		if width <= 0 || width > self.config.max_signal_width {
			error!("Signal {} has width {} which is out of range", sig.name(), width);
			return Err(ElabMessageKind::InvalidSignalWidth(width));
		}

		// Evaluate dimensions
		let mut dimensions = Vec::new();
		for dim_expr in &sig.dimensions {
			dim_expr.validate(&assumptions.clone(), &validation_scope)?;
			let dim = dim_expr.eval(&assumptions)?.try_into_i64()?;
			if dim <= 0 || dim > self.config.max_array_dimension {
				error!("Signal {} has dimension {} which is out of range", sig.name(), dim);
				return Err(ElabMessageKind::InvalidArrayDimension(dim));
			}
			dimensions.push(dim as usize);
		}

		// Verify total array size
		let total_fields = dimensions.iter().product::<usize>();
		if total_fields > self.config.max_array_size {
			error!(
				"Signal {} has {} fields which is out of range",
				sig.name(),
				total_fields
			);
			return Err(ElabMessageKind::InvalidArraySize(total_fields));
		}

		// ID for the generated signal
		let gen_id = GeneratedSignalId {
			id,
			pass_id,
			instance_id: ext_instance_id,
		};

		// Insert all array fields into the graph and elab signal array if the signal is internal
		if ext_instance_id.is_none() {
			if dimensions.len() > 0 {
				debug!("Array signal - will insert total of {} signal nodes", total_fields);
				for i in 0..total_fields {
					let sig_ref = GeneratedSignalRef {
						id: gen_id,
						index: Some(i as u32),
					};

					self.comb_graph.add_node(sig_ref);
					let sig_existed = self
						.elab_signals
						.insert(sig_ref, ElabSignal::new(width as u32))
						.is_some();
					assert!(
						!sig_existed,
						"An elab signal already exists for this generated signal ref"
					)
				}
			}
			else {
				debug!("Scalar signal - inserting into graph and registering elab signals");
				let sig_ref = GeneratedSignalRef {
					id: gen_id,
					index: None,
				};

				self.comb_graph.add_node(sig_ref);
				let sig_existed = self
					.elab_signals
					.insert(sig_ref, ElabSignal::new(width as u32))
					.is_some();
				assert!(
					!sig_existed,
					"An elab signal already exists for this generated signal ref"
				)
			}
		}

		// Register the generated signal
		let gen_sig = GeneratedSignal {
			width: width as u32,
			dimensions,
			total_fields,
			instance_id: ext_instance_id,
		};

		let sig_exists = self.signals.insert(gen_id.clone(), gen_sig).is_some();
		assert!(!sig_exists, "Generated signal already exists!");

		Ok(gen_id)
	}

	pub fn declare_signal(
		&mut self,
		id: SignalId,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<GeneratedSignalId, ElabMessageKind> {
		let sig = self.design.get_signal(id).expect("signal not in design");
		let pass_id = self.get_scope_pass_id(sig.parent_scope);
		let scope_handle = self.design.get_scope_handle(sig.parent_scope).unwrap();

		self.declare_signal_impl(id, scope_handle, Some(pass_id), None, assumptions)
	}

	pub fn declare_ext_interface_signal(
		&mut self,
		module: ModuleHandle,
		id: SignalId,
		instance_id: usize,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<GeneratedSignalId, ElabMessageKind> {
		self.declare_signal_impl(id, module.scope(), None, Some(instance_id), assumptions)
	}
}
