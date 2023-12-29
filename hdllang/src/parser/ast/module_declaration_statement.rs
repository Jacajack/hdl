mod pretty_printable;
mod variable_block_declaration;
mod variable_declaration_statement;
use hirn::design::ModuleHandle;

pub use variable_block_declaration::*;
pub use variable_declaration_statement::*;

use crate::analyzer::*;
use crate::lexer::IdTable;
use crate::parser::ast::SourceLocation;
use crate::SourceSpan;

use super::{DirectDeclarator, TypeDeclarator, TypeQualifier};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum ModuleDeclarationStatement {
	VariableDeclarationStatement(VariableDeclarationStatement),
	VariableBlock(ModuleDeclarationVariableBlock),
}

impl ModuleDeclarationStatement {
	pub fn create_variable_declaration(
		&self,
		already_created: AlreadyCreated,
		nc_table: &crate::core::NumericConstantTable,
		comment_table: &crate::lexer::CommentTable,
		id_table: &IdTable,
		context: &mut Box<LocalAnalyzerContext>,
		handle: &mut ModuleHandle,
	) -> miette::Result<()> {
		use ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement(declaration) => declaration.create_variable_declaration(
				already_created,
				nc_table,
				id_table,
				comment_table,
				context,
				handle,
			),
			VariableBlock(block) => {
				block.create_variable_declaration(already_created, nc_table, id_table, comment_table, context, handle)
			},
		}
	}
}

impl SourceLocation for ModuleDeclarationStatement {
	fn get_location(&self) -> SourceSpan {
		use self::ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement(declaration) => declaration.location,
			VariableBlock(block) => block.location,
		}
	}
}
