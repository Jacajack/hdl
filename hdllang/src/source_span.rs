pub struct SourceSpan {
	pub start: usize,
	pub end: usize,
}

impl SourceSpan {
	fn new(start: usize, end: usize) -> SourceSpan {
		SourceSpan {
			start,
			end,
		}
	}
}

impl From<(usize, usize)> for SourceSpan {
	fn from(pair: (usize, usize)) -> Self {
		SourceSpan::new(pair.0, pair.1)
	}
}

