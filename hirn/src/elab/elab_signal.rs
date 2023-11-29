use std::ops::Shl;

use num_bigint::BigUint;

#[derive(Clone, Debug)]
pub struct SparseSignalMask {
	width: u32,
	mask: BigUint,
}

impl SparseSignalMask {
	fn new(width: u32) -> Self {
		assert_ne!(width, 0, "SignalMask width cannot be 0");
		Self {
			width,
			mask: BigUint::from(0u32),
		}
	}

	fn new_range(width: u32, lsb: u32, msb: u32) -> Self {
		let mut mask = Self::new(width);
		mask.set_bits(lsb, msb);
		mask
	}

	fn width(&self) -> u32 {
		self.width
	}

	fn set_bit(&mut self, bit: u32) -> bool {
		assert!(bit < self.width);
		let value = self.mask.bit(bit as u64);
		self.mask.set_bit(bit as u64, true);
		value
	}

	fn get_bit(&self, bit: u32) -> bool {
		assert!(bit < self.width);
		self.mask.bit(bit as u64)
	}

	fn set_bits(&mut self, lsb: u32, msb: u32) -> SparseSignalMask {
		assert!(lsb < self.width);
		assert!(msb < self.width);
		assert!(lsb <= msb);

		let mut conflict_mask = SparseSignalMask::new(self.width);

		// TODO optimize using bitwise magic
		for i in lsb..=msb {
			let conflict = self.set_bit(i);
			if conflict {
				conflict_mask.set_bit(i);
			}
		}

		conflict_mask
	}

	fn combine(&mut self, other: &Self) -> SparseSignalMask {
		assert_eq!(self.width, other.width);
		let conflict = self.mask.clone() & &other.mask;
		self.mask |= &other.mask;
		Self {
			width: self.width,
			mask: conflict,
		}
	}

	fn and(&self, other: &Self) -> SparseSignalMask {
		assert_eq!(self.width, other.width);
		let conflict = self.mask.clone() & &other.mask;
		Self {
			width: self.width,
			mask: conflict,
		}
	}

	fn negated(&self) -> SparseSignalMask {
		let ones_mask = BigUint::from(1u32).shl(self.width) - 1u32;
		let negated_mask = ones_mask - &self.mask;
		Self {
			width: self.width,
			mask: negated_mask,
		}
	}

	fn is_empty(&self) -> bool {
		self.mask == BigUint::from(0u32)
	}

	fn is_full(&self) -> bool {
		self.mask.count_ones() == (self.width as u64)
	}
}

#[derive(Clone, Debug)]
pub enum SignalMask {
	Full { width: u32, set: bool },
	Sparse(SparseSignalMask),
}

impl SignalMask {
	pub fn new(width: u32) -> Self {
		assert_ne!(width, 0, "SignalMask width cannot be 0");
		Self::Full { width, set: false }
	}

	pub fn new_set(width: u32) -> Self {
		assert_ne!(width, 0, "SignalMask width cannot be 0");
		Self::Full { width, set: true }
	}

	fn normalize(&mut self) {
		use SignalMask::*;
		match self {
			Full { .. } => {},
			Sparse(m) => {
				if m.is_empty() {
					*self = Full {
						width: m.width(),
						set: false,
					};
				}
				else if m.is_full() {
					*self = Full {
						width: m.width(),
						set: true,
					};
				}
			},
		}
	}

	pub fn width(&self) -> u32 {
		use SignalMask::*;
		match self {
			Full { width, .. } => *width,
			Sparse(m) => m.width(),
		}
	}

	pub fn get_bit(&self, bit: u32) -> bool {
		assert!(bit < self.width());

		use SignalMask::*;
		match self {
			Full { width: _, set } => *set,
			Sparse(m) => m.get_bit(bit),
		}
	}

	pub fn set_all(&mut self) -> SignalMask {
		use SignalMask::*;
		match self {
			Full { width, set: true } => Self::new_set(*width),
			Full { width, set: false } => {
				let w = *width;
				*self = Full { width: w, set: true };
				Self::new(w)
			},
			Sparse(m) => {
				let conflict = m.clone();
				*self = Self::Full {
					width: conflict.width(),
					set: true,
				};
				Self::Sparse(conflict)
			},
		}
	}

	pub fn set_bits(&mut self, lsb: u32, msb: u32) -> SignalMask {
		assert!(lsb < self.width());
		assert!(msb < self.width());
		assert!(lsb <= msb);

		use SignalMask::*;
		let conflict = match self {
			Full { width: _, set: true } => Self::Sparse(SparseSignalMask::new_range(self.width(), lsb, msb)),
			Full { width: _, set: false } => {
				*self = Self::Sparse(SparseSignalMask::new_range(self.width(), lsb, msb));
				Self::new(self.width())
			},
			Sparse(m) => {
				let conflict = m.set_bits(lsb, msb);
				if conflict.is_empty() {
					Self::new(self.width())
				}
				else {
					Self::Sparse(conflict)
				}
			},
		};

		self.normalize();
		conflict
	}

	pub fn combine(&mut self, other: &SignalMask) -> SignalMask {
		assert!(self.width() == other.width());

		use SignalMask::*;
		let conflict = match (&self, other) {
			(Full { width: _, set: false }, rhs) => {
				*self = rhs.clone();
				Self::new(rhs.width())
			},

			(Full { width: _, set: true }, rhs) => rhs.clone(),

			(
				Sparse(_),
				Full {
					width: rhs_w,
					set: false,
				},
			) => Self::new(*rhs_w),

			(
				Sparse(lhs_mask),
				Full {
					width: rhs_w,
					set: true,
				},
			) => {
				let conflict = lhs_mask.clone();
				*self = Self::new_set(*rhs_w);
				Self::Sparse(conflict)
			},

			(Sparse(lhs_mask), Sparse(rhs_mask)) => Self::Sparse(lhs_mask.clone().combine(rhs_mask)),
		};

		self.normalize();
		conflict
	}

	pub fn and(&self, other: SignalMask) -> SignalMask {
		assert!(self.width() == other.width());

		use SignalMask::*;
		match (self, other) {
			(Full { set: true, .. }, Full { set: true, .. }) => Self::new_set(self.width()),
			(Full { .. }, Full { .. }) => Self::new(self.width()),
			(Full { set: true, .. }, Sparse(m)) => Self::Sparse(m),
			(Sparse(m), Full { set: true, .. }) => Self::Sparse(m.clone()),
			(Full { set: false, .. }, Sparse(_m)) => Self::new(self.width()),
			(Sparse(_m), Full { set: false, .. }) => Self::new(self.width()),
			(Sparse(lhs_mask), Sparse(rhs_mask)) => Self::Sparse(lhs_mask.clone().and(&rhs_mask)),
		}
	}

	pub fn negated(&self) -> SignalMask {
		use SignalMask::*;
		match self {
			Full { width, set } => Self::Full {
				width: *width,
				set: !*set,
			},
			Sparse(m) => Self::Sparse(m.negated()),
		}
	}

	pub fn is_full(&self) -> bool {
		// Could be optimized by asserting that we're always normalized
		use SignalMask::*;
		match self {
			Full { set, .. } => *set,
			Sparse(m) => m.is_full(),
		}
	}

	pub fn is_empty(&self) -> bool {
		// Could be optimized by asserting that we're always normalized
		use SignalMask::*;
		match self {
			Full { set, .. } => !*set,
			Sparse(m) => m.is_empty(),
		}
	}

	pub fn count_ones(&self) -> u32 {
		use SignalMask::*;
		match self {
			Full { set, .. } => {
				if *set {
					self.width()
				}
				else {
					0
				}
			},
			Sparse(m) => m.mask.count_ones() as u32,
		}
	}

	pub fn zeros_summary(&self) -> SignalMaskSummary {
		SignalMaskSummary::new_zero_summary(self)
	}

	pub fn ones_summary(&self) -> SignalMaskSummary {
		SignalMaskSummary::new_one_summary(self)
	}
}

#[derive(Clone, Debug)]
pub enum SignalMaskSummary {
	Empty,
	Full,
	Single(u32),
	Ranges(Vec<(u32, u32)>),
}

impl SignalMaskSummary {
	fn new_zero_summary(mask: &SignalMask) -> Self {
		if mask.is_empty() {
			Self::Full
		}
		else if mask.is_full() {
			Self::Empty
		}
		else {
			Self::generic_summary(mask, false)
		}
	}

	fn new_one_summary(mask: &SignalMask) -> Self {
		if mask.is_empty() {
			Self::Empty
		}
		else if mask.is_full() {
			Self::Full
		}
		else {
			Self::generic_summary(mask, true)
		}
	}

	fn generic_summary(mask: &SignalMask, desired: bool) -> Self {
		let mut ranges = vec![];

		let mut group_start = None;
		for i in 0..mask.width() {
			let bit = mask.get_bit(i);
			if bit == desired {
				if group_start.is_none() {
					group_start = Some(i);
				}
			}

			if bit != desired && group_start.is_some() {
				ranges.push((group_start.unwrap(), i - 1));
				group_start = None;
			}
		}

		if group_start.is_some() {
			ranges.push((group_start.unwrap(), mask.width() - 1));
		}

		// One bit - degenerate case
		if ranges.len() == 1 && ranges[0].0 == ranges[0].1 {
			Self::Single(ranges[0].0)
		}
		else {
			Self::Ranges(ranges)
		}
	}
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum ElabSignalMarkAction {
	Read,
	Drive,
	Unuse,
}

#[derive(Clone, Debug)]
pub struct ElabSignal {
	conflict_mask: SignalMask,
	driven_mask: SignalMask,
	read_mask: SignalMask,
	unused_mask: SignalMask,
}

impl ElabSignal {
	pub fn new(width: u32) -> Self {
		Self {
			conflict_mask: SignalMask::new(width),
			driven_mask: SignalMask::new(width),
			read_mask: SignalMask::new(width),
			unused_mask: SignalMask::new(width),
		}
	}

	pub fn drive(&mut self) -> SignalMask {
		let conflict = self.driven_mask.set_all();
		self.conflict_mask.combine(&conflict);
		conflict
	}

	pub fn read(&mut self) {
		self.read_mask.set_all();
	}

	pub fn unuse(&mut self) {
		self.unused_mask.set_all();
	}

	pub fn drive_bits(&mut self, lsb: u32, msb: u32) -> SignalMask {
		let conflict = self.driven_mask.set_bits(lsb, msb);
		self.conflict_mask.combine(&conflict);
		conflict
	}

	pub fn read_bits(&mut self, lsb: u32, msb: u32) {
		self.read_mask.set_bits(lsb, msb);
	}

	pub fn unuse_bits(&mut self, lsb: u32, msb: u32) {
		self.unused_mask.set_bits(lsb, msb);
	}

	pub fn mark(&mut self, action: ElabSignalMarkAction) -> SignalMask {
		use ElabSignalMarkAction::*;
		match action {
			Drive => self.drive(),
			Read => {
				self.read();
				SignalMask::new(self.width())
			},
			Unuse => {
				self.unuse();
				SignalMask::new(self.width())
			},
		}
	}

	pub fn mark_bits(&mut self, action: ElabSignalMarkAction, lsb: u32, msb: u32) -> SignalMask {
		use ElabSignalMarkAction::*;
		match action {
			Drive => self.drive_bits(lsb, msb),
			Read => {
				self.read_bits(lsb, msb);
				SignalMask::new(self.width())
			},
			Unuse => {
				self.unuse_bits(lsb, msb);
				SignalMask::new(self.width())
			},
		}
	}

	pub fn is_fully_driven(&self) -> bool {
		self.driven_mask.is_full()
	}

	pub fn is_fully_read(&self) -> bool {
		self.read_mask.is_full()
	}

	pub fn is_read(&self) -> bool {
		!self.read_mask.is_empty()
	}

	pub fn has_conflicts(&self) -> bool {
		!self.conflict_mask.is_empty()
	}

	pub fn width(&self) -> u32 {
		self.read_mask.width()
	}

	pub fn conflict_mask(&self) -> &SignalMask {
		&self.conflict_mask
	}

	pub fn driven_mask(&self) -> &SignalMask {
		&self.driven_mask
	}

	pub fn read_mask(&self) -> &SignalMask {
		&self.read_mask
	}

	pub fn undriven_read_mask(&self) -> SignalMask {
		self.driven_mask.negated().and(self.read_mask().clone())
	}

	pub fn undriven_read_summary(&self) -> SignalMaskSummary {
		self.undriven_read_mask().ones_summary()
	}

	pub fn conflict_summary(&self) -> SignalMaskSummary {
		self.conflict_mask.ones_summary()
	}

	pub fn undriven_summary(&self) -> SignalMaskSummary {
		self.driven_mask.zeros_summary()
	}

	pub fn unread_summary(&self) -> SignalMaskSummary {
		// actually_read = unused | read = !(~unused & ~read)
		self.unused_mask
			.negated()
			.and(self.read_mask().negated())
			.ones_summary()
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn basic_test() {
		let mut sig = ElabSignal::new(32);
		assert!(!sig.is_fully_driven());
		assert!(!sig.is_fully_read());
		assert!(!sig.has_conflicts());

		sig.drive_bits(0, 1);
		assert!(!sig.is_fully_driven());
		assert!(!sig.is_fully_read());
		assert!(!sig.has_conflicts());

		sig.drive_bits(2, 31);
		assert!(sig.is_fully_driven());
		assert!(!sig.is_fully_read());
		assert!(!sig.has_conflicts());

		sig.read_bits(0, 16);
		assert!(!sig.is_fully_read());

		sig.read_bits(0, 31);
		assert!(sig.is_fully_read());
		assert!(!sig.has_conflicts());

		sig.drive();
		assert!(sig.has_conflicts());
		assert!(sig.is_fully_driven());
	}
}
