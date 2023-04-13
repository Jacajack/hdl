use std::ops::Range;

/// Indicates a region in the source code
#[derive(Clone, Copy, Debug)]
pub struct SourceSpan {
	start: usize,
	end: usize,
}

impl SourceSpan {
	/// Creates a new source span from offset and length
	pub fn new(offset: usize, len: usize) -> SourceSpan {
		SourceSpan {
			start: offset,
			end: offset + len,
		}
	}

	pub fn new_between(start: usize, end: usize) -> SourceSpan {
		SourceSpan { start, end }
	}

	/// Creates a new span from a Range type
	pub fn new_from_range(range: &Range<usize>) -> SourceSpan {
		SourceSpan {
			start: range.start,
			end: range.end,
		}
	}

	/// Returns offset of the span
	pub fn offset(&self) -> usize {
		self.start
	}

	/// Returns start of the span
	pub fn start(&self) -> usize {
		self.offset()
	}

	/// Returns end of the span
	pub fn end(&self) -> usize {
		self.offset() + self.len()
	}

	/// Returns length of the span
	pub fn len(&self) -> usize {
		self.end - self.start
	}
}

impl From<(usize, usize)> for SourceSpan {
	fn from(pair: (usize, usize)) -> Self {
		SourceSpan::new(pair.0, pair.1)
	}
}

impl From<SourceSpan> for miette::SourceSpan {
	fn from(span: SourceSpan) -> Self {
		(span.start, span.len()).into()
	}
}
