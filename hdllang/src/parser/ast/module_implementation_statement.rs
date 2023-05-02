use crate::parser::ast::{
	AssignmentOpcode, Expression, PortBindStatement, SourceLocation, VariableBlock, VariableDeclaration,
	VariableDefinition, RangeExpression
};
use crate::{lexer::{IdTableKey,CommentTableKey}, SourceSpan};
use std::fmt::{Debug, Error, Formatter};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub enum ModuleImplementationStatement {
	VariableDeclarationStatement {
		declaration: VariableDeclaration,
		location: SourceSpan,
	},
	VariableBlock {
		block: Box<VariableBlock>,
		location: SourceSpan,
	},
	VariableDefinitionStatement {
		definition: VariableDefinition,
		location: SourceSpan,
	},
	AssignmentStatement {
		lhs: Box<Expression>,
		assignment_opcode: AssignmentOpcode,
		rhs: Box<Expression>,
		location: SourceSpan,
	},
	IfStatement {
		condition: Box<Expression>,
		if_statement: Box<ModuleImplementationStatement>,
		location: SourceSpan,
	},
	IfElseStatement {
		condition: Box<Expression>,
		if_statement: Box<ModuleImplementationStatement>,
		else_statement: Box<ModuleImplementationStatement>,
		location: SourceSpan,
	},
	IterationStatement {
		id: IdTableKey,
		range: RangeExpression,
		statement: Box<ModuleImplementationStatement>,
		location: SourceSpan,
	},
	InstantiationStatement {
		metadata: Vec<CommentTableKey>,
		id1: IdTableKey,
		id2: IdTableKey,
		port_bind: Vec<PortBindStatement>,
		location: SourceSpan,
	},
	ModuleImplementationBlockStatement {
		statements: Vec<Box<ModuleImplementationStatement>>,
		location: SourceSpan,
	},
}
impl Debug for ModuleImplementationStatement {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		use self::ModuleImplementationStatement::*;
		match &self {
			VariableDeclarationStatement {
				declaration,
				..
			} => write!(fmt, "\n{:?};", declaration),
			VariableBlock { block, location: _ } => write!(fmt, "\n{:?}", block),
			VariableDefinitionStatement {
				definition,
				..
			} => write!(fmt, "\n{:?};", definition),
			AssignmentStatement {
				lhs,
				assignment_opcode,
				rhs,
				..
			} => write!(fmt, "\n{:?} {:?} {:?};", lhs, assignment_opcode, rhs),
			IterationStatement {
				id: _,
				range,
				statement,
				..
			} => write!(fmt, "\nfor(foo in {:?})\n{:?}", range, statement),
			InstantiationStatement {
				id1,
				id2,
				port_bind,
				..
			} => {
				write!(fmt, "\n{:?} {:?}{{\n",id1,id2)?;
				for port_bind_statement in port_bind.into_iter() {
					write!(fmt, "{:?},\n", port_bind_statement)?;
				}
				write!(fmt, "}};")
			},
			ModuleImplementationBlockStatement {
				statements,
				..
			} => {
				write!(fmt, "\n{{")?;
				for statement in statements.into_iter() {
					write!(fmt, "{:?}\n", statement)?;
				}
				write!(fmt, "}}")
			},
			IfStatement {
				condition,
				if_statement,
				..
			} => write!(fmt, "\nif({:?}){:?}", condition, if_statement),
			IfElseStatement {
				condition,
				if_statement,
				else_statement,
				..
			} => write!(
				fmt,
				"\nif({:?}){:?}\nelse{:?}",
				condition, if_statement, else_statement
			),
		}
	}
}
impl SourceLocation for ModuleImplementationStatement {
	fn get_location(&self) -> SourceSpan {
		use self::ModuleImplementationStatement::*;
		match *self {
			VariableDeclarationStatement { location, .. } => location,
			VariableBlock { location, .. } => location,
			VariableDefinitionStatement { location, .. } => location,
			AssignmentStatement { location, .. } => location,
			IfStatement { location, .. } => location,
			IfElseStatement { location, .. } => location,
			IterationStatement { location, .. } => location,
			InstantiationStatement { location, .. } => location,
			ModuleImplementationBlockStatement { location, .. } => location,
		}
	}
}
