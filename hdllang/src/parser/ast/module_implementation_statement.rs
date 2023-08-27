mod pretty_printable;

use crate::parser::ast::{
	AssignmentOpcode, Expression, ImportPath, PortBindStatement, RangeExpression, SourceLocation, VariableBlock, VariableDefinition,
};
use crate::{
	lexer::{CommentTableKey, IdTableKey},
	SourceSpan,
};


#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct AssignmentStatement {
	pub lhs: Expression,
	pub assignment_opcode: AssignmentOpcode,
	pub rhs: Expression,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct IfElseStatement {
	pub condition: Expression,
	pub if_statement: Box<ModuleImplementationStatement>,
	pub else_statement: Option<Box<ModuleImplementationStatement>>,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct IterationStatement {
	pub id: IdTableKey,
	pub range: RangeExpression,
	pub statement: Box<ModuleImplementationStatement>,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct InstantiationStatement {
	pub metadata: Vec<CommentTableKey>,
	pub module_name: ImportPath,
	pub instance_name: IdTableKey,
	pub port_bind: Vec<PortBindStatement>,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct ModuleImplementationBlockStatement {
	pub statements: Vec<ModuleImplementationStatement>,
	pub location: SourceSpan,
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum ModuleImplementationStatement {
	VariableBlock (VariableBlock),
	VariableDefinition (VariableDefinition),
	AssignmentStatement (AssignmentStatement),
	IfElseStatement (IfElseStatement),
	IterationStatement (IterationStatement),
	InstantiationStatement (InstantiationStatement),
	ModuleImplementationBlockStatement (ModuleImplementationBlockStatement),
}

impl SourceLocation for ModuleImplementationStatement {
	fn get_location(&self) -> SourceSpan {
		use self::ModuleImplementationStatement::*;
		match self {
			VariableBlock (block) => block.location,
			VariableDefinition (definition) => definition.location,
			AssignmentStatement (assignment_statement) => assignment_statement.location,
			IfElseStatement (if_else) => if_else.location,
			IterationStatement (iteration) => iteration.location,
			InstantiationStatement (instantation) => instantation.location,
			ModuleImplementationBlockStatement (block) => block.location,
		}
	}
}
