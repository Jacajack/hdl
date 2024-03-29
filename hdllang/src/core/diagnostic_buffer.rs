use crate::compiler_diagnostic::CompilerDiagnostic;

#[derive(Clone)]
pub struct DiagnosticBuffer {
	pub buffer: Vec<CompilerDiagnostic>,
	error_buffer: Vec<CompilerDiagnostic>,
}

impl DiagnosticBuffer {
	pub fn push_diagnostic(&mut self, diag: CompilerDiagnostic) {
		self.buffer.push(diag);
	}
	pub fn push_error(&mut self, diag: CompilerDiagnostic) {
		self.error_buffer.push(diag);
	}
	pub fn contains_errors(&self) -> bool {
		return !self.error_buffer.is_empty();
	}
	pub fn print_diagnostics(self, file_name: String, source_code: String, json_report: bool) -> miette::Result<()> {
		if !self.buffer.is_empty() && !json_report {
			eprintln!("During elaboration the following diagnostics were generated:");
		}

		for diag in self.buffer {
			if json_report {
				println!(
					"{:?}",
					miette::Report::new(diag)
						.with_source_code(miette::NamedSource::new(file_name.clone(), source_code.clone()))
				);
			}
			else {
				eprintln!(
					"{:?}",
					miette::Report::new(diag)
						.with_source_code(miette::NamedSource::new(file_name.clone(), source_code.clone()))
				);
			}
		}
		if self.error_buffer.is_empty() {
			return Ok(());
		}
		if !json_report {
			eprintln!("During elaboration the following errors were generated:");
		}
		for i in 0..self.error_buffer.len() - 1 {
			let diag = self.error_buffer[i].clone();
			if json_report {
				println!(
					"{:?}",
					miette::Report::new(diag)
						.with_source_code(miette::NamedSource::new(file_name.clone(), source_code.clone()))
				);
			}
			else {
				eprintln!(
					"{:?}",
					miette::Report::new(diag)
						.with_source_code(miette::NamedSource::new(file_name.clone(), source_code.clone()))
				);
			}
		}
		if json_report {
			println!(
				"{:?}",
				miette::Report::new(self.error_buffer.last().unwrap().clone())
					.with_source_code(miette::NamedSource::new(file_name.clone(), source_code.clone()))
			)
		}
		return Err(miette::Report::new(self.error_buffer.last().unwrap().clone())
			.with_source_code(miette::NamedSource::new(file_name, source_code))
			.into());
	}
	pub fn new() -> Self {
		Self {
			buffer: vec![],
			error_buffer: vec![],
		}
	}
}
