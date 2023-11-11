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
	pub fn is_input(&self)->bool{
		use Direction::*;
		match self {
			Input(_) => true,
			_ => false,
		}
	}
}
