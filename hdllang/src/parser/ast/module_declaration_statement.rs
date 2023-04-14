use crate::parser::ast::{SourceLocation, VariableBlock, VariableDeclaration};
use crate::SourceSpan;
use std::fmt::{Debug, Error, Formatter};

pub enum ModuleDeclarationStatement {
	VariableDeclarationStatement {
		declaration: VariableDeclaration,
		location: SourceSpan,
	},
	VariableBlock {
		block: Box<VariableBlock>,
		location: SourceSpan,
	},
}
impl Debug for ModuleDeclarationStatement {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::ModuleDeclarationStatement::*;
		match &self {
			VariableDeclarationStatement {
				declaration,
				location: _,
			} => write!(fmt, "{:?};", declaration),
			VariableBlock { block, location: _ } => write!(fmt, "{{{:?}\n}}", block),
		}
	}
}
impl SourceLocation for ModuleDeclarationStatement {
	fn get_location(&self) -> SourceSpan {
		use self::ModuleDeclarationStatement::*;
		match *self {
			VariableDeclarationStatement { location, .. } => location,
			VariableBlock { location, .. } => location,
		}
	}
}
