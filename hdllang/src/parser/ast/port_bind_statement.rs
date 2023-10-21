mod pretty_printable;

use hirn::design::ScopeHandle;

use crate::analyzer::{GlobalAnalyzerContext, LocalAnalyzerContex};
use crate::parser::ast::{Expression, SourceLocation, VariableDeclaration};
use crate::{lexer::IdTableKey, SourceSpan};

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdWithDeclaration {
	pub id: IdTableKey,
	pub declaration: VariableDeclaration,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdWithExpression {
	pub id: IdTableKey,
	pub expression: Expression,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct OnlyId {
	pub id: IdTableKey,
	pub location: SourceSpan,
}
impl SourceLocation for PortBindStatement {
	fn get_location(&self) -> SourceSpan {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.location,
			IdWithExpression(id_with_expression) => id_with_expression.location,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.location,
		}
	}
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum PortBindStatement {
	OnlyId(OnlyId),
	IdWithExpression(IdWithExpression),
	IdWithDeclaration(IdWithDeclaration),
}

impl PortBindStatement {
	pub fn get_id(&self) -> IdTableKey {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.id,
			IdWithExpression(id_with_expression) => id_with_expression.id,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.id,
		}
	}
	pub fn location(&self) -> SourceSpan {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => only_id.location,
			IdWithExpression(id_with_expression) => id_with_expression.location,
			IdWithDeclaration(id_with_declaration) => id_with_declaration.location,
		}
	}
	pub fn codegen_pass(
		&self,
		ctx: &GlobalAnalyzerContext,
		local_ctx: &LocalAnalyzerContex,
		api_scope: &mut ScopeHandle,
		current_scope: usize,
	) -> miette::Result<hirn::design::Expression> {
		use self::PortBindStatement::*;
		match self {
			OnlyId(only_id) => {
				let id = only_id.id;
				let signal_id = local_ctx.scope.get_api_id(current_scope, &id).unwrap();
				Ok(hirn::design::Expression::Signal(signal_id.into()))
			},
			IdWithExpression(id_with_expression) => {
				let expression = id_with_expression.expression.codegen(
					ctx.nc_table,
					&ctx.id_table,
					current_scope,
					&local_ctx.scope,
					Some(&local_ctx.nc_widths),
				)?;
				Ok(expression)
			},
			IdWithDeclaration(id_with_declaration) => {
				todo!("PortBindStatement::IdWithDeclaration") //FIXME
			},
		}
	}
}
