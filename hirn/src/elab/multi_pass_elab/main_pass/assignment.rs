use std::sync::Arc;

use log::error;

use crate::{design::{ScopeHandle, Expression, HasSensitivity, WidthExpression, Evaluates}, elab::{ElabAssumptionsBase, ElabMessageKind}};

use super::MainPassCtx;

impl MainPassCtx {
	pub fn assign_signals(
		&mut self,
		scope: ScopeHandle,
		assumptions: Arc<dyn ElabAssumptionsBase>,
		lhs: &Expression,
		lhs_external: bool,
		rhs: &Expression,
		rhs_external: bool,
	) -> Result<(), ElabMessageKind> {
		let driven_bits = lhs.try_drive_bits().ok_or(ElabMessageKind::NotDrivable)?;
		let driven_sig = self.design.get_signal(driven_bits.signal()).expect("LHS signal not in design");

		// Ignore generic assignments
		if driven_sig.sensitivity().is_generic() {
			return Ok(());
		}

		// Validate both expressions
		lhs.validate(&assumptions.clone(), &scope)?;
		rhs.validate(&assumptions.clone(), &scope)?;

		// Get all dependencies
		let rhs_dependencies = rhs.get_used_slice_ranges();
		let mut lhs_dependencies = Vec::new();
		for index in driven_bits.slice().indices() {
			lhs_dependencies.extend(index.get_used_slice_ranges());
		}

		// Check widths
		let lhs_width = lhs.width()?.eval(&assumptions)?.try_into_u64()?;
		let rhs_width = rhs.width()?.eval(&assumptions)?.try_into_u64()?;
		if lhs_width != rhs_width {
			error!(
				"Assignment width mismatch ({}B = {}B): LHS: {:?}, RHS is {:?}",
				lhs_width, rhs_width,
				lhs, rhs,
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
		
		// TODO signal graph insert dependency

		Ok(())
	}
}
