mod pretty_printable;
mod if_else_statement;
mod assignment_statement;
mod module_implementation_block_statement;
mod iteration_statement;
mod instantiation_statement;

use hirn::design::ScopeHandle;
pub use instantiation_statement::*;
pub use iteration_statement::*;
pub use module_implementation_block_statement::*;
pub use assignment_statement::*;
pub use if_else_statement::*;
use crate::analyzer::{GlobalAnalyzerContext, LocalAnalyzerContext};
use crate::parser::ast::{
	AssignmentOpcode, Expression, ImportPath, PortBindStatement, RangeExpression, SourceLocation, VariableBlock,
	VariableDefinition,
};
use crate::SourceSpan;




#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum ModuleImplementationStatement {
	VariableBlock(VariableBlock),
	VariableDefinition(VariableDefinition),
	AssignmentStatement(AssignmentStatement),
	IfElseStatement(IfElseStatement),
	IterationStatement(IterationStatement),
	InstantiationStatement(InstantiationStatement),
	ModuleImplementationBlockStatement(ModuleImplementationBlockStatement),
}

impl SourceLocation for ModuleImplementationStatement {
	fn get_location(&self) -> SourceSpan {
		use self::ModuleImplementationStatement::*;
		match self {
			VariableBlock(block) => block.location,
			VariableDefinition(definition) => definition.location,
			AssignmentStatement(assignment_statement) => assignment_statement.location,
			IfElseStatement(if_else) => if_else.location,
			IterationStatement(iteration) => iteration.location,
			InstantiationStatement(instantation) => instantation.location,
			ModuleImplementationBlockStatement(block) => block.location,
		}
	}
}

impl ModuleImplementationStatement {
	pub fn first_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContext,
		scope_id: usize,
	) -> miette::Result<()> {
		local_ctx.scope_map.insert(self.get_location(), scope_id);
		log::info!("Inserting scope id {} for {:?}", scope_id, self.get_location(),);
		use ModuleImplementationStatement::*;
		match self {
			VariableBlock(block) => block.analyze(ctx, local_ctx, crate::analyzer::AlreadyCreated::new(), scope_id),
			VariableDefinition(definition) => definition.analyze(crate::analyzer::AlreadyCreated::new(), ctx, local_ctx, scope_id),
			AssignmentStatement(assignment) => assignment.first_pass(scope_id, ctx, local_ctx),
			IfElseStatement(conditional) => conditional.first_pass(scope_id, ctx, local_ctx),
			IterationStatement(iteration) => iteration.first_pass(scope_id, ctx, local_ctx),
			InstantiationStatement(inst) => inst.first_pass(scope_id, ctx, local_ctx),
			ModuleImplementationBlockStatement(block) => block.analyze(ctx, local_ctx, scope_id),
		}
	}
	pub fn codegen_pass(
		&self,
		ctx: &mut GlobalAnalyzerContext,
		local_ctx: &mut LocalAnalyzerContext,
		api_scope: &mut ScopeHandle,
	) -> miette::Result<()> {
		log::info!("Reading scope id for {:?}", self.get_location());
		let scope_id = local_ctx.scope_map.get(&self.get_location()).unwrap().to_owned();
		let _additional_ctx = crate::analyzer::AdditionalContext::new(
			local_ctx.nc_widths.clone(),
			local_ctx.array_or_bus.clone(),
			local_ctx.casts.clone(),
		);
		use ModuleImplementationStatement::*;
		match self {
			VariableBlock(block) => block.codegen_pass(ctx, local_ctx, api_scope),
			VariableDefinition(definition) => definition.codegen_pass(ctx, local_ctx, api_scope),
			AssignmentStatement(assignment) => assignment.codegen_pass(ctx, local_ctx, scope_id, api_scope),
			IfElseStatement(conditional) => conditional.codegen_pass(ctx, local_ctx, scope_id, api_scope),
			IterationStatement(for_stmt) => for_stmt.codegen_pass(ctx, local_ctx, scope_id, api_scope),
			InstantiationStatement(inst) => inst.codegen_pass(ctx, local_ctx, scope_id, api_scope),
			ModuleImplementationBlockStatement(block) => block.codegen_pass(ctx, local_ctx, api_scope),
		}
	}
}
