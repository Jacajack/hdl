use crate::analyzer::{AlreadyCreated, ModuleDeclared, ModuleImplementationScope, SemanticError, LocalAnalyzerContext, GlobalAnalyzerContext};
use crate::core::{CommentTableKey, IdTableKey, SourceSpan};
use crate::parser::ast::ModuleDeclarationStatement;
use crate::ProvidesCompilerDiagnostic;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct ModuleDeclaration {
	pub metadata: Vec<CommentTableKey>,
	pub id: IdTableKey,
	pub statements: Vec<ModuleDeclarationStatement>,
	pub location: SourceSpan,
}

impl ModuleDeclaration {
	pub fn analyze(
		&self,
		ctx: &mut GlobalAnalyzerContext,
	) -> miette::Result<()> {
		use log::*;

		debug!(
			"Analyzing module declaration {:?}",
			ctx.id_table.get_by_key(&self.id).unwrap()
		);
		if let Some(module) = ctx.modules_declared.get(&self.id) {
			return Err(miette::Report::new(
				SemanticError::MultipleModuleDeclaration
					.to_diagnostic_builder()
					.label(module.location, "Module with this name is already declared here.")
					.label(self.location, "Module with this name is already declared.")
					.build(),
			));
		}
		let mut handle = ctx.design
			.new_module(ctx.id_table.get_by_key(&self.id).unwrap())
			.unwrap();
		if !self.metadata.is_empty() {
			let mut comment = String::new();
			for com in &self.metadata {
				comment.push_str(ctx.comment_table.get_by_key(&com).unwrap());
			}
			handle.comment(comment.as_str());
		}
		let mut local_ctx = LocalAnalyzerContext::new(self.id, ModuleImplementationScope::new());
		debug!(
			"Registering variables for module declaration {:?}:",
			ctx.id_table.get_by_key(&self.id).unwrap()
		);
		for statement in &self.statements {
			statement.create_variable_declaration(
				AlreadyCreated::new(),
				ctx,
				&mut local_ctx,
				&mut handle,
			)?;
			if local_ctx.scope.is_generic() {
				local_ctx.scope.transorm_to_generic();
			}
		}

		if local_ctx.scope.is_generic() {
			info!("Module {:?} is generic", ctx.id_table.get_by_key(&self.id).unwrap());
		}

		let is_generic = local_ctx.scope.is_generic();

		let new_module = ModuleDeclared {
			name: self.id,
			context: local_ctx,
			handle,
			is_generic,
			location: self.location,
			instantiates: Vec::new(),
		};
		ctx.modules_declared.insert(self.id, new_module);
		Ok(())
	}
}
