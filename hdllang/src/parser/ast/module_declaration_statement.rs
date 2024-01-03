mod pretty_printable;
mod variable_block_declaration;
mod variable_declaration_statement;
use hirn::design::ModuleHandle;

pub use variable_block_declaration::*;
pub use variable_declaration_statement::*;

use crate::analyzer::*;
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
		global_ctx: &mut GlobalAnalyzerContext,
		context: &mut Box<LocalAnalyzerContext>,
		handle: &mut ModuleHandle,
	) -> miette::Result<()> {
		use ModuleDeclarationStatement::*;
		match self {
			VariableDeclarationStatement(declaration) => declaration.create_variable_declaration(
				already_created,
				global_ctx,
				context,
				handle,
			),
			VariableBlock(block) => {
				block.create_variable_declaration(already_created, global_ctx, context, handle)
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
