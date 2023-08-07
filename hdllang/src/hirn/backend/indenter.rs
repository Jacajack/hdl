use std::fmt;

pub struct Indenter<'stream> {
	output: &'stream mut dyn fmt::Write,
	indent_str: String,
}

impl<'stream> Indenter<'stream> {
	pub fn new(output: &'stream mut dyn fmt::Write, indent_str: String) -> Indenter<'stream> {
		Self {
			output,
			indent_str,
		}
	}

	pub fn new_with_tabs(output: &'stream mut dyn fmt::Write, indent_level: usize) -> Indenter<'stream> {
		Self::new(output, "\t".repeat(indent_level))
	}
}

impl<'stream> fmt::Write for Indenter<'stream> {
	fn write_str(&mut self, s: &str) -> Result<(), std::fmt::Error> {
		for (i, line) in s.split('\n').enumerate() {
			let prepend_newline = i != 0;
			let prepend_indent = !line.is_empty();

			self.output.write_fmt(format_args!(
				"{}{}{}",
				if prepend_newline { "\n" } else { "" },
				if prepend_indent { &self.indent_str } else { "" },
				line
			))?;
		}

		Ok(())
	}
}

macro_rules! indented {
	($output: expr) => {
		{
			let out = &mut $output;
			Indenter::new_with_tabs(out, 1)
		}
	};

	($output: expr, $levels: expr) => {
		{
			let out = &mut $output;
			Indenter::new_with_tabs(out, $levels)
		}
	};

}

pub(crate) use indented;

#[cfg(test)]
mod test {
	use super::*;
	use std::fmt::Write;

	#[test]
	fn basic_test() {
		let mut output = String::new();
		write!(indented!(output), "A\nB\n\nC\n").unwrap();
		assert_eq!(output, "\tA\n\tB\n\n\tC\n")
	}
}