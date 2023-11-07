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

	fn mask_or(&mut self, other: &Self) -> SparseSignalMask {
		assert_eq!(self.width, other.width);
		let conflict = self.mask.clone() & &other.mask;
		self.mask |= &other.mask;
		Self {
			width: self.width,
			mask: conflict,
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
			Full { width, set: false } => Self::new(*width),
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

	pub fn mask_or(&mut self, other: &SignalMask) -> SignalMask {
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

			(Sparse(lhs_mask), Sparse(rhs_mask)) => Self::Sparse(lhs_mask.clone().mask_or(rhs_mask)),
		};

		self.normalize();
		conflict
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
}

pub struct ElabSignal {
	conflict_mask: SignalMask,
	driven_mask: SignalMask,
	read_mask: SignalMask,
}

impl ElabSignal {
	pub fn new(width: u32) -> Self {
		Self {
			conflict_mask: SignalMask::new(width),
			driven_mask: SignalMask::new(width),
			read_mask: SignalMask::new(width),
		}
	}

	pub fn drive(&mut self) -> SignalMask {
		let conflict = self.driven_mask.set_all();
		self.conflict_mask.mask_or(&conflict);
		conflict
	}

	pub fn read(&mut self) {
		self.read_mask.set_all();
	}

	pub fn drive_bits(&mut self, lsb: u32, msb: u32) -> SignalMask {
		let conflict = self.driven_mask.set_bits(lsb, msb);
		self.conflict_mask.mask_or(&conflict);
		conflict
	}

	pub fn read_bits(&mut self, lsb: u32, msb: u32) {
		self.read_mask.set_bits(lsb, msb);
	}

	pub fn is_fully_driven(&self) -> bool {
		self.driven_mask.is_full()
	}

	pub fn is_fully_read(&self) -> bool {
		self.read_mask.is_full()
	}

	pub fn has_conflicts(&self) -> bool {
		!self.conflict_mask.is_empty()
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
