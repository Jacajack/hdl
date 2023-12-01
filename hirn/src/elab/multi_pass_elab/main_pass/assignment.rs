use std::sync::Arc;

use log::error;

use crate::{
	design::{Evaluates, Expression, HasSensitivity, ScopeHandle, WidthExpression},
	elab::{ElabAssumptionsBase, ElabMessageKind},
};

use super::MainPassCtx;

impl MainPassCtx {
	fn assign_arrays(
		&mut self,
		assumptions: Arc<dyn ElabAssumptionsBase>,
		lhs: &Expression,
		lhs_ext_instance: Option<usize>,
		rhs: &Expression,
		rhs_ext_instance: Option<usize>,
	) -> Result<(), ElabMessageKind> {
		let lhs_slice_range = lhs.try_drive_bits().ok_or(ElabMessageKind::NotDrivable)?;
		let rhs_slice_range = rhs.try_drive_bits().ok_or(ElabMessageKind::NotDrivable)?;
		let lhs_ref =
			self.get_int_ext_generated_signal_ref(lhs_slice_range.slice(), lhs_ext_instance, assumptions.clone())?;
		let rhs_ref =
			self.get_int_ext_generated_signal_ref(rhs_slice_range.slice(), rhs_ext_instance, assumptions.clone())?;

		if lhs_slice_range.slice().rank() != 0 || rhs_slice_range.slice().rank() != 0 {
			error!("Array assignment with non-zero rank");
			return Err(ElabMessageKind::PartialArrayAssignment {
				lhs: Box::new(lhs_ref),
				rhs: Box::new(rhs_ref),
			});
		}

		let lhs_sig = self.get_generated_signal(&lhs_ref.gen_id());
		let rhs_sig = self.get_generated_signal(&rhs_ref.gen_id());

		assert!(lhs_sig.is_array());
		assert!(rhs_sig.is_array());

		if lhs_sig.dimensions() != rhs_sig.dimensions() {
			error!(
				"Mismatched dimensions in array assignment {:?} vs {:?}",
				lhs_sig.dimensions(),
				rhs_sig.dimensions()
			);
			return Err(ElabMessageKind::ArraySizeMismatch {
				lhs: Box::new(lhs_ref),
				rhs: Box::new(rhs_ref),
				lhs_dimensions: lhs_sig.dimensions().into(),
				rhs_dimensions: rhs_sig.dimensions().into(),
			});
		}

		if lhs_ext_instance.is_none() && rhs_ext_instance.is_none() {
			for i in 0..lhs_sig.total_fields() {
				self.comb_graph
					.add_edge(lhs_ref.with_index(i as u32), rhs_ref.with_index(i as u32), ());
			}
		}

		if lhs_ext_instance.is_none() {
			self.drive_signal(&lhs_slice_range, assumptions.clone())?;
		}

		if rhs_ext_instance.is_none() {
			self.read_signal(&rhs_slice_range, assumptions.clone())?;
		}

		Ok(())
	}

	pub fn assign_signals(
		&mut self,
		scope: ScopeHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>,
		lhs: &Expression,
		lhs_ext_instance: Option<usize>,
		rhs: &Expression,
		rhs_ext_instance: Option<usize>,
	) -> Result<(), ElabMessageKind> {
		// debug!("Assigning {:?} = {:?}", lhs, rhs);

		let lhs_external = lhs_ext_instance.is_some();
		let rhs_external = rhs_ext_instance.is_some();
		let driven_bits = lhs.try_drive_bits().ok_or(ElabMessageKind::NotDrivable)?;
		let driven_sig = self
			.design
			.get_signal(driven_bits.signal())
			.expect("LHS signal not in design");

		// Ignore generic assignments
		if driven_sig.sensitivity().is_generic() {
			return Ok(());
		}

		// Validate both expressions
		if !lhs_external {
			lhs.validate(&assumptions.clone(), &scope)?;
		}

		if !rhs_external {
			rhs.validate(&assumptions.clone(), &scope)?;
		}

		// Array assignment case
		if driven_sig.is_array() && driven_bits.slice().rank() == 0 {
			return self.assign_arrays(assumptions, lhs, lhs_ext_instance, rhs, rhs_ext_instance);
		}

		// Get all dependencies
		let mut rhs_dependencies = rhs.get_used_slice_ranges();
		let mut lhs_dependencies = Vec::new();
		for index in driven_bits.slice().indices() {
			lhs_dependencies.extend(index.get_used_slice_ranges());
		}

		// Filter out generics from LHS dependencies
		lhs_dependencies.retain(|range| {
				let sig = self
					.design
					.get_signal(range.signal())
					.expect("LHS dependency not in design");
				!sig.is_generic()
			});

		// We should not have any LHS dependencies on non-generic signals
		assert!(
			lhs_dependencies.is_empty(),
			"LHS shall not depend on a non-generic signal"
		);

		// Filter out generics from RHS dependencies
		rhs_dependencies.retain(|range| {
				let sig = self
					.design
					.get_signal(range.signal())
					.expect("RHS dependency not in design");
				!sig.is_generic()
			});

		// debug!("LHS width: {:?}", lhs.width());
		// debug!("RHS width: {:?}", rhs.width());

		// Check widths
		let lhs_width = lhs.width()?.eval(&assumptions)?.try_into_u64()?;
		let rhs_width = rhs.width()?.eval(&assumptions)?.try_into_u64()?;
		if lhs_width != rhs_width {
			error!(
				"Assignment width mismatch ({}b = {}b): LHS: {:?}, RHS is {:?}",
				lhs_width, rhs_width, lhs, rhs,
			);

			return Err(ElabMessageKind::WidthMismatch {
				lhs: Box::new(self.get_generated_signal_ref(driven_bits.slice(), assumptions.clone())?),
				lhs_width,
				rhs_width,
			});
		}

		// Mark LHS as driven
		if !lhs_external {
			self.drive_signal(&driven_bits, assumptions.clone())?;

			// Mark dependencies as read
			for range in &lhs_dependencies {
				self.read_signal(range, assumptions.clone())?;
			}
		}
		else {
			assert!(lhs_dependencies.is_empty(), "LHS external but has dependencies");
		}

		// Mark dependencies as read if RHS is internal
		if !rhs_external {
			for range in &rhs_dependencies {
				self.read_signal(range, assumptions.clone())?;
			}
		}

		if !lhs_external && !rhs_external {
			let lhs_gen_ref = self.get_generated_signal_ref(driven_bits.slice(), assumptions.clone())?;
			for range in &rhs_dependencies {
				let rhs_gen_ref = self.get_generated_signal_ref(range.slice(), assumptions.clone())?;
				self.comb_graph.add_edge(lhs_gen_ref, rhs_gen_ref, ());
			}
		}

		Ok(())
	}
}
