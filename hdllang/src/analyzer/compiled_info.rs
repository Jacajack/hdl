use serde::{Deserialize, Serialize};
extern crate sha256;
use crate::{CompilerError, ProvidesCompilerDiagnostic};
#[derive(Serialize, Deserialize, Debug)]
pub struct CompiledInfo {
	name: String,
	hash: String,
	dependencies: Option<Vec<String>>,
	packages: String,
}
// naming!
impl CompiledInfo {
	pub fn new(file_name: String) -> miette::Result<CompiledInfo> {
		use std::fs;
		let file =
			fs::read_to_string(&file_name).map_err(|_| CompilerError::FileNotFound(file_name).to_diagnostic())?;
		let compiled: CompiledInfo =
			serde_json::from_str(&file).map_err(|e| CompilerError::JsonError(e).to_diagnostic())?;
		Ok(compiled)
	}
	pub fn compare_hashes(&self, code: String) -> bool {
		use sha256::digest;
		digest(code) == self.hash
	}
	pub fn save_to_file(self) -> miette::Result<()> {
		use std::fs;
		let json = serde_json::to_string_pretty(&self).map_err(|e| CompilerError::JsonError(e).to_diagnostic())?;
		fs::write(format!("{}.com", self.name), json).map_err(|e| CompilerError::IoError(e).to_diagnostic())?;
		Ok(())
	}
}
