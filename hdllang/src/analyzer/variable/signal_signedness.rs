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

#[cfg(test)]
mod test {
	use super::*;

	fn span() -> SourceSpan {
		SourceSpan::new_between(0, 0)
	}

	#[test]
	fn test_names() {
		assert_eq!(SignalSignedness::Signed(span()).name(), "signed");
		assert_eq!(SignalSignedness::Unsigned(span()).name(), "unsigned");
	}

	#[test]
	#[should_panic]
	fn test_name_no_signedness() {
		SignalSignedness::NoSignedness.name();
	}

	#[test]
	fn test_location() {
		assert_eq!(SignalSignedness::Signed(span()).location(), Some(&span()));
		assert_eq!(SignalSignedness::Unsigned(span()).location(), Some(&span()));
		assert_eq!(SignalSignedness::NoSignedness.location(), None);
	}

	#[test]
	fn test_is_none() {
		assert!(SignalSignedness::NoSignedness.is_none());
		assert!(!SignalSignedness::Signed(span()).is_none());
		assert!(!SignalSignedness::Unsigned(span()).is_none());
	}

	#[test]
	fn test_is_signed() {
		assert!(SignalSignedness::Signed(span()).is_signed());
		assert!(!SignalSignedness::Unsigned(span()).is_signed());
		assert!(!SignalSignedness::NoSignedness.is_signed());
	}

	#[test]
	fn test_is_unsigned() {
		assert!(!SignalSignedness::Signed(span()).is_unsigned());
		assert!(SignalSignedness::Unsigned(span()).is_unsigned());
		assert!(!SignalSignedness::NoSignedness.is_unsigned());
	}
}
