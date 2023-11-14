use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Direction {
	Input(SourceSpan),
	Output(SourceSpan),
	Tristate(SourceSpan),
	None,
}
impl Direction {
	pub fn name(&self) -> &'static str {
		use Direction::*;
		match self {
			Input(_) => "input",
			Output(_) => "output",
			Tristate(_) => "tristate",
			None => panic!("No direction"),
		}
	}
	pub fn location(&self) -> Option<&SourceSpan> {
		match self {
			Direction::Input(x) => Some(x),
			Direction::Output(x) => Some(x),
			Direction::Tristate(x) => Some(x),
			Direction::None => None,
		}
	}
	pub fn is_input(&self) -> bool {
		use Direction::*;
		match self {
			Input(_) => true,
			_ => false,
		}
	}
}

#[cfg(test)]
mod test {

	fn span() -> SourceSpan {
		SourceSpan::new_between(0, 0)
	}
	use super::*;

	#[test]
	fn test_names() {
		assert_eq!(Direction::Input(span()).name(), "input");
		assert_eq!(Direction::Output(span()).name(), "output");
		assert_eq!(Direction::Tristate(span()).name(), "tristate");
	}

	#[test]
	#[should_panic]
	fn test_name_no_direction() {
		Direction::None.name();
	}

	#[test]
	fn test_location() {
		assert_eq!(Direction::Input(span()).location(), Some(&span()));
		assert_eq!(Direction::Output(span()).location(), Some(&span()));
		assert_eq!(Direction::Tristate(span()).location(), Some(&span()));
		assert_eq!(Direction::None.location(), None);
	}
}
