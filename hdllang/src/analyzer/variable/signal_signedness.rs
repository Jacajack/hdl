use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignalSignedness {
	Signed(SourceSpan),
	Unsigned(SourceSpan),
	NoSignedness,
}
impl SignalSignedness {
	pub fn is_none(&self) -> bool {
		use SignalSignedness::*;
		match self {
			NoSignedness => true,
			_ => false,
		}
	}
	pub fn is_signed(&self) -> bool {
		use SignalSignedness::*;
		match self {
			Signed(_) => true,
			_ => false,
		}
	}
	pub fn is_unsigned(&self) -> bool {
		use SignalSignedness::*;
		match self {
			Unsigned(_) => true,
			_ => false,
		}
	}
	pub fn name(&self) -> &'static str {
		use SignalSignedness::*;
		match self {
			Signed(_) => "signed",
			Unsigned(_) => "unsigned",
			NoSignedness => panic!("No signedness"),
		}
	}
	pub fn location(&self) -> Option<&SourceSpan> {
		use SignalSignedness::*;
		match self {
			Signed(x) => Some(x),
			Unsigned(x) => Some(x),
			NoSignedness => None,
		}
	}
}
