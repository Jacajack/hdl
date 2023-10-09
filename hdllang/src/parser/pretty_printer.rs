use crate::{
	core::NumericConstant,
	core::{
		numeric_constant_table::{NumericConstantTable, NumericConstantTableKey},
		CommentTable, CommentTableKey, CompilerError, IdTable, IdTableKey,
	},
	ProvidesCompilerDiagnostic,
};
use std::io::Write;
pub struct PrettyPrinterContext<'extras> {
	indentation_level: usize,
	id_table: &'extras IdTable,
	comment_table: &'extras CommentTable,
	constant_table: &'extras NumericConstantTable,
	newline_given: bool,
	pub after_brackets: bool,
	pub afer_else: bool,
	output: Box<dyn Write>,
}
impl<'extras> PrettyPrinterContext<'extras> {
	pub fn new(
		id_table: &'extras IdTable,
		comment_table: &'extras CommentTable,
		constant_table: &'extras NumericConstantTable,
		output: Box<dyn Write>,
	) -> Self {
		Self {
			indentation_level: 0,
			id_table,
			comment_table,
			constant_table,
			newline_given: false,
			after_brackets: false,
			afer_else: false,
			output,
		}
	}
	pub fn write_indent(&mut self, s: &str) -> miette::Result<()> {
		match self.newline_given {
			true => write!(self.output, "{}{}", "\t".repeat(self.indentation_level), s)
				.map_err(|e| CompilerError::IoError(e).to_diagnostic())?,
			false => self.write(s)?,
		}
		self.newline_given = false;
		Ok(())
	}
	pub fn writeln(&mut self, s: &str) -> miette::Result<()> {
		writeln!(self.output, "{}", s).map_err(|e| CompilerError::IoError(e).to_diagnostic())?;
		self.newline_given = true;
		Ok(())
	}
	pub fn write(&mut self, s: &str) -> miette::Result<()> {
		write!(self.output, "{}", s).map_err(|e| CompilerError::IoError(e).to_diagnostic())?;
		self.newline_given = false;
		Ok(())
	}
	pub fn write_opt_newline(&mut self, s: &str) -> miette::Result<()> {
		match self.newline_given {
			true => self.write_indent(s),
			false => {
				self.writeln("")?;
				self.write_indent(s)
			},
		}
	}
	pub fn get_id(&self, id: IdTableKey) -> &str {
		self.id_table.get_by_key(&id).unwrap()
	}
	pub fn get_comment(&self, id: CommentTableKey) -> &str {
		self.comment_table.get_by_key(&id).unwrap()
	}
	pub fn get_numeric_constant(&self, id: NumericConstantTableKey) -> &NumericConstant {
		self.constant_table.get_by_key(&id).unwrap()
	}
	pub fn increase_indent(&mut self) {
		//println!("increase_indent");
		self.indentation_level += 1;
	}
	pub fn decrease_indent(&mut self) {
		//println!("decrease_indent");
		self.indentation_level -= 1;
	}
}
pub trait PrettyPrintable {
	fn pretty_print(&self, ctx: &mut PrettyPrinterContext) -> miette::Result<()>;
}
