mod pretty_printable;

use crate::parser::ast::{SourceLocation, VariableBlock, VariableDeclaration};
use crate::SourceSpan;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Error, Formatter};

#[derive(Serialize, Deserialize)]
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
