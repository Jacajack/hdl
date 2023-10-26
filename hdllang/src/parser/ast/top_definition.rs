mod pretty_printable;

use hirn::design::DesignHandle;
use log::info;

use crate::analyzer::{AlreadyCreated, ModuleDeclared, ModuleImplementationScope, SemanticError, Variable};
use crate::lexer::IdTable;
use crate::parser::ast::{ImportPath, ModuleDeclarationStatement, ModuleImplementationStatement, SourceLocation};
use crate::ProvidesCompilerDiagnostic;
use crate::{lexer::CommentTableKey, lexer::IdTableKey, SourceSpan};
use std::collections::HashMap;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum TopDefinition {
	ModuleDeclaration(ModuleDeclaration),
	ModuleImplementation(ModuleImplementation),
	PackageDeclaration(PackageDeclaration),
	UseStatement(UseStatement),
}
pub trait Scope {
	fn get_variable(&self, name: &IdTableKey) -> Option<Variable>;
}
//#[derive(Debug, Clone)]
//pub struct DeclarationScope{
//	pub signals: HashMap<IdTableKey, Variable>,
//	pub signals_id: HashMap<IdTableKey, SignalId>,
//	pub generics: HashMap<IdTableKey, Variable>,
//}
//impl DeclarationScope{
//	pub fn new() -> Self{
//		DeclarationScope{
//			signals: HashMap::new(),
//			signals_id: HashMap::new(),
//			generics: HashMap::new(),
//		}
//	}
//	pub fn is_declared(&self, name: &IdTableKey) -> bool{
//		self.signals.contains_key(name) || self.generics.contains_key(name)
//	}
//	pub fn declare(&mut self, variable: Variable)-> miette::Result<()>{
//		if let Some(var) = self.signals.get(&variable.name){
//			return Err(miette::Report::new(
//				SemanticError::DuplicateVariableDeclaration
//					.to_diagnostic_builder()
//					.label(variable.location, "Second declaration of this variable.")
//					.label(var.location, "Signal with this name is already declared here.")
//					.build(),
//			));
//		}
//		if let Some(var) = self.generics.get(&variable.name){
//			return Err(miette::Report::new(
//				SemanticError::DuplicateVariableDeclaration
//					.to_diagnostic_builder()
//					.label(variable.location, "Second declaration of this variable.")
//					.label(var.location, "Generic with this name is already declared here.")
//					.build(),
//			));
//		}
//		match variable.kind{
//			crate::analyzer::VariableKind::Signal(_) => self.signals.insert(variable.name, variable),
//			crate::analyzer::VariableKind::Generic(_) => self.generics.insert(variable.name, variable),
//		};
//		Ok(())
//	}
//	pub fn is_generic(&self) -> bool{
//		self.generics.len() > 0
//	}
//	pub fn analyze(&self, id_table: &IdTable) -> miette::Result<()>{
//		for (_, var) in &self.signals{
//			use crate::analyzer::VariableKind::*;
//			match &var.kind{
//    			Signal(signal) => {
//					use SignalSensitivity::*;
//					match &signal.sensitivity{
//        				Async(_) | Clock(_) | Const(_) | NoSensitivity => (),
//        				Comb(list, qualifier_location) => {
//							for expr in &list.list{
//								let name = expr.clock_signal;
//								if name == var.name{
//									return Err(miette::Report::new(SemanticError::VariableReferencingItself
//										.to_diagnostic_builder()
//										.label(var.location, format!("This variable's {:?} sync list contains itself", id_table.get_by_key(&var.name).unwrap()).as_str())
//										.label(*qualifier_location, "\"comb\" qualifier must have at most consists of one variable and its negation")
//										.build(),
//									));
//								}
//								match self.signals.get(&name){
//									None => return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder()
//										.label(*qualifier_location, format!("This variable's {:?} qualifier \"comb\" references a variable {:?} that is not declared", id_table.get_by_key(&var.name).unwrap(), id_table.get_by_key(&name).unwrap()).as_str())
//										.build())),
//									Some(var2) => {
//										if ! var.is_clock() {
//											return  Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder()
//												.label(var.location, format!("This variable's {:?} sync list contains non-clock signals", id_table.get_by_key(&var.name).unwrap()).as_str())
//												.label(*qualifier_location, "This is the sync list")
//												.label(var2.location, format!("This variable {:?} is not a clock signal", id_table.get_by_key(&var2.name).unwrap()).as_str())
//												.build()))
//										}
//									},
//								}
//							}
//						},
//        				Sync(list, qualifier_location) => {
//							match list.list.len(){
//								1 => {
//									let name = list.list[0].clock_signal;
//									match self.signals.get(&name){
//										None => return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder()
//										.label(*qualifier_location, format!("This variable's {:?} qualifier \"comb\" references a variable {:?} that is not declared", id_table.get_by_key(&var.name).unwrap(), id_table.get_by_key(&name).unwrap()).as_str())
//										.build())),
//										Some(var2) => {
//											if ! var.is_clock() {
//												return  Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder()
//													.label(var.location, format!("This variable's {:?} sync list contains non-clock signals",id_table.get_by_key(&var.name).unwrap()).as_str())
//													.label(*qualifier_location, "This is the sync list")
//													.label(var2.location, format!("This variable {:?} is not a clock signal", id_table.get_by_key(&var2.name).unwrap()).as_str())
//													.build()))
//											}
//										},
//									}
//								},
//								2 => {
//									let first = list.list[0].clock_signal;
//									let second = list.list[1].clock_signal;
//									match self.signals.get(&first){
//										None => return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder()
//										.label(*qualifier_location, format!("This variable's {:?} qualifier \"comb\" references a variable {:?} that is not declared", id_table.get_by_key(&var.name).unwrap(), id_table.get_by_key(&first).unwrap()).as_str())
//										.build())),
//										Some(var2) => {
//											if ! var.is_clock() {
//												return  Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder()
//													.label(var.location, format!("This variable's {:?} sync list contains non-clock signals",id_table.get_by_key(&var.name).unwrap()).as_str())
//													.label(*qualifier_location, "This is the sync list")
//													.label(var2.location, format!("This variable {:?} is not a clock signal", id_table.get_by_key(&var2.name).unwrap()).as_str())
//													.build()))
//											}
//										},
//									}
//									if first != second {
//										return Err(miette::Report::new(SemanticError::ForbiddenExpressionInSyncOrComb
//											.to_diagnostic_builder()
//											.label(var.location, format!("This variable's {:?} sync list contains two different variables",id_table.get_by_key(&var.name).unwrap()).as_str())
//											.label(*qualifier_location, "\"sync\" qualifier must have at most consists of one variable and its negation")
//											.build(),
//										));
//									} else if list.list[0].on_rising != list.list[1].on_rising {
//										return Err(miette::Report::new(SemanticError::ForbiddenExpressionInSyncOrComb
//											.to_diagnostic_builder()
//											.label(var.location, format!("This variable's {:?} sync list contains two variables with different negation",id_table.get_by_key(&var.name).unwrap()).as_str())
//											.label(*qualifier_location, "\"sync\" qualifier must have at most consists of one variable and its negation")
//											.build(),
//										));
//									}
//								},
//								_ => return Err(miette::Report::new(
//									SemanticError::ForbiddenExpressionInSyncOrComb
//										.to_diagnostic_builder()
//										.label(
//										var.location,
//											format!(
//												"This variable's {:?} sync list contains more than two variables",
//												id_table.get_by_key(&var.name).unwrap()
//											)
//											.as_str(),
//										)
//										.label(
//											*qualifier_location,
//											"\"sync\" qualifier must have at most consists of one variable and its negation",
//										)
//										.build())),
//							}
//						},
//    				}
//				},
//    			Generic(_) => unreachable!(),
//			}
//		}
//		Ok(())
//	}
//}
//impl Scope for DeclarationScope{
//    fn get_variable(&self, name: &IdTableKey) -> Option<Variable> {
//        self.signals.get(name).cloned()
//    }
//}
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
		design_handle: &mut DesignHandle,
		id_table: &IdTable,
		nc_table: &crate::lexer::NumericConstantTable,
		modules_declared: &mut HashMap<IdTableKey, ModuleDeclared>,
	) -> miette::Result<()> {
		use log::debug;
		debug!(
			"Analyzing module declaration {:?}",
			id_table.get_by_key(&self.id).unwrap()
		);
		if let Some(module) = modules_declared.get(&self.id) {
			return Err(miette::Report::new(
				SemanticError::MultipleModuleDeclaration
					.to_diagnostic_builder()
					.label(module.location, "Module with this name is already declared here.")
					.label(self.location, "Module with this name is already declared.")
					.build(),
			));
		}
		let mut handle = design_handle
			.new_module(id_table.get_by_key(&self.id).unwrap())
			.unwrap();
		//let mut scope = ModuleDeclarationScope::new();
		let mut new_scope = ModuleImplementationScope::new();
		debug!(
			"Registering variables for module declaration {:?}:",
			id_table.get_by_key(&self.id).unwrap()
		);
		for statement in &self.statements {
			statement.create_variable_declaration(AlreadyCreated::new(), nc_table, id_table, &mut new_scope, &mut handle)?;
			if new_scope.is_generic() {
				new_scope.transorm_to_generic();
			}
		}
		if new_scope.is_generic() {
			info!("Module {:?} is generic", id_table.get_by_key(&self.id).unwrap());
			//new_scope = ModuleImplementationScope::new();
		}
		debug!(
			"Module {:?} in api: {:?}",
			id_table.get_by_key(&self.id).unwrap(),
			handle
		);
		//for statement in &self.statements {
		//	statement.analyze(CombinedQualifiers::new(), &mut scope, id_table)?;
		//}

		let is_generic = new_scope.is_generic();
		let m = ModuleDeclared {
			name: self.id,
			scope: new_scope,
			handle,
			is_generic,
			location: self.location,
			instantiates: Vec::new(),
		};
		modules_declared.insert(self.id, m);
		Ok(())
	}
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct ModuleImplementation {
	pub metadata: Vec<CommentTableKey>,
	pub id: IdTableKey,
	pub statement: ModuleImplementationStatement,
	pub location: SourceSpan,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct PackageDeclaration {
	pub metadata: Vec<CommentTableKey>,
	pub path: ImportPath,
	pub location: SourceSpan,
}
impl PackageDeclaration {
	pub fn analyze(
		&self,
		id_table: &IdTable,
		path_from_root: &String,
		present_files: &mut HashMap<String, String>,
	) -> miette::Result<String> {
		use crate::parser::ast::Modules::*;
		use crate::CompilerError;
		use log::debug;
		use sha256::try_digest;
		use std::path::Path;
		match &self.path.modules {
			All => {
				return Err(miette::Report::new(
					SemanticError::MultiplePackageDeclaration
						.to_diagnostic_builder()
						.label(self.location, "You should declare only one package at a time.")
						.build(),
				))
			},
			Specific { modules } => {
				if modules.len() != 1 {
					return Err(miette::Report::new(
						SemanticError::MultiplePackageDeclaration
							.to_diagnostic_builder()
							.label(self.location, "You should declare only one package at a time.")
							.build(),
					));
				}

				let path = self.path.into_paths(id_table, &path_from_root).get(0).unwrap().clone();

				if !Path::new(&path).exists() {
					return Err(miette::Report::new(
						CompilerError::FileNotFound(path.clone())
							.to_diagnostic_builder()
							.label(self.location, &format!("Package not found: {}", path))
							.build(),
					));
				}

				let hash = try_digest(&path).unwrap();
				debug!("File path: {}", path);
				debug!("Hash: {}", hash);
				debug!("Present files: {:?}", present_files);

				if let Some(file_name) = present_files.get(&hash) {
					debug!("File already packaged: {}", file_name);
					return Err(miette::Report::new(
						SemanticError::FilePackagedMultipleTimes
							.to_diagnostic_builder()
							.label(self.location, create_label_message(file_name).as_str())
							.build(),
					));
				}

				debug!("File not packaged yet");
				present_files.insert(hash, path.clone());
				Ok(path.clone())
			},
		}
	}
}

fn create_label_message(file_name: &String) -> String {
	match file_name.as_str() {
		"root" => String::from("File already explicitly included via compiler command line argument"),
		_ => format!("File already packaged in {}", file_name),
	}
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct UseStatement {
	pub metadata: Vec<CommentTableKey>,
	pub path: ImportPath,
	pub location: SourceSpan,
}

impl SourceLocation for TopDefinition {
	fn get_location(&self) -> SourceSpan {
		use self::TopDefinition::*;
		match self {
			ModuleImplementation(implementation) => implementation.location,
			PackageDeclaration(package) => package.location,
			UseStatement(use_statement) => use_statement.location,
			ModuleDeclaration(declaration) => declaration.location,
		}
	}
}
