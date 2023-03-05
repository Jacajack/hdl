pub struct SymbolKey {

}

pub struct SymbolEntry {
	name: String,
	// value: HdlType,
}

pub enum SymbolError {
	Internal
}

pub trait SymbolTable {
	fn register(name: &str) -> Result<SymbolKey, SymbolError>;
}

pub struct BasicSymbolTable {

}

impl SymbolTable for BasicSymbolTable {
	fn register(name: &str) -> Result<SymbolKey, SymbolError> {
		// TODO
		Err(SymbolError::Internal)
	}
}