use std::sync::Arc;

use log::{debug, error};

use crate::{
	design::{Evaluates, SignalId, SignalSliceRange, HasSensitivity},
	elab::{multi_pass_elab::signal_graph_pass::GeneratedSignalRef, ElabAssumptionsBase, ElabMessageKind, ElabSignal},
};

use super::SignalGraphPassCtx;
struct SignalSliceRangeEvalResult<'a> {
	elab_sig: &'a mut ElabSignal,
	lsb_msb: Option<(i64, i64)>,
}

impl SignalGraphPassCtx {
	fn eval_slice_range(
		&mut self,
		range: SignalSliceRange,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<SignalSliceRangeEvalResult, ElabMessageKind> {
		let sig_ref = self.get_generated_signal_ref(range.slice(), assumptions.clone())?;
		let elab_sig = self
			.elab_signals
			.get_mut(&sig_ref)
			.expect("Sliced signal not registered in elab");

		if range.is_full() {
			Ok(SignalSliceRangeEvalResult {
				elab_sig,
				lsb_msb: None,
			})
		}
		else {
			let lsb_expr = range.lsb().unwrap();
			let msb_expr = range.msb().unwrap();
			let lsb_val = lsb_expr.eval(&assumptions)?;
			let msb_val = msb_expr.eval(&assumptions)?;
			let lsb = lsb_val.try_into_i64()?;
			let msb = msb_val.try_into_i64()?;

			if lsb < 0 || msb < 0 || msb < lsb || msb >= (elab_sig.width() as i64) {
				error!(
					"Signal {:?} (width {}) is driven with invalid range {}:{}",
					range.signal(),
					elab_sig.width(),
					msb,
					lsb
				);
				return Err(ElabMessageKind::InvalidSignalBitRange);
			}

			Ok(SignalSliceRangeEvalResult {
				elab_sig,
				lsb_msb: Some((lsb, msb)),
			})
		}
	}

	fn drive_signal_slice(
		&mut self,
		range: &SignalSliceRange,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		debug!("Driving signal {:?}", range);

		let result = self.eval_slice_range(range.clone(), assumptions)?;
		if let Some((lsb, msb)) = result.lsb_msb {
			result.elab_sig.drive_bits(lsb as u32, msb as u32);
		}
		else {
			result.elab_sig.drive();
		}

		Ok(())
	}

	fn read_signal_slice(
		&mut self,
		range: &SignalSliceRange,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		debug!("Marking signal as read {:?}", range);

		let result = self.eval_slice_range(range.clone(), assumptions)?;
		if let Some((lsb, msb)) = result.lsb_msb {
			result.elab_sig.read_bits(lsb as u32, msb as u32);
		}
		else {
			result.elab_sig.read();
		}

		Ok(())
	}

	fn drive_array(&mut self, id: SignalId) -> Result<(), ElabMessageKind> {
		debug!("Driving full array signal {:?}", id);
		let gen_id = self.get_generated_signal_id(id);
		let gen_sig = self.get_generated_signal(&gen_id);

		for i in 0..gen_sig.total_fields() {
			let sig_ref = GeneratedSignalRef::new(gen_id, Some(i as u32));
			let elab_sig = self
				.elab_signals
				.get_mut(&sig_ref)
				.expect("Sliced signal not registered in elab");
			elab_sig.drive();
		}

		Ok(())
	}

	fn read_array(&mut self, id: SignalId) -> Result<(), ElabMessageKind> {
		debug!("Driving full array signal {:?}", id);
		let gen_id = self.get_generated_signal_id(id);
		let gen_sig = self.get_generated_signal(&gen_id);

		for i in 0..gen_sig.total_fields() {
			let sig_ref = GeneratedSignalRef::new(gen_id, Some(i as u32));
			let elab_sig = self
				.elab_signals
				.get_mut(&sig_ref)
				.expect("Sliced signal not registered in elab");
			elab_sig.read();
		}

		Ok(())
	}

	pub fn drive_signal(
		&mut self,
		range: &SignalSliceRange,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		let sig = self.design.get_signal(range.signal()).unwrap();
		
		// Ignore generic signals
		if sig.is_generic() {
			return Ok(());
		}

		// We don't care about any indices on the slice because they must be generic

		// TODO validate rank
		if sig.is_array() && range.slice().rank() == 0 {
			self.drive_array(range.signal())
		}
		else {
			self.drive_signal_slice(range, assumptions)
		}
	}

	pub fn read_signal(
		&mut self,
		range: &SignalSliceRange,
		assumptions: Arc<dyn ElabAssumptionsBase>,
	) -> Result<(), ElabMessageKind> {
		let sig = self.design.get_signal(range.signal()).unwrap();
		
		// Ignore generic signals
		if sig.is_generic() {
			return Ok(());
		}

		// We don't care about any indices on the slice because they must be generic

		// TODO validate rank
		if sig.is_array() && range.slice().rank() == 0 {
			self.read_array(range.signal())
		}
		else {
			self.read_signal_slice(range, assumptions)
		}
	}
}
