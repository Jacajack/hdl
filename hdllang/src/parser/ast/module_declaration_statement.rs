mod pretty_printable;


use num_bigint::BigInt;

use crate::analyzer::*;
use crate::lexer::IdTable;
use crate::parser::ast::SourceLocation;
use crate::lexer::CommentTableKey;
use crate::{ProvidesCompilerDiagnostic, SourceSpan};

use super::{DirectDeclarator, TypeDeclarator, TypeQualifier, TypeSpecifier};
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct VariableDeclarationStatement {
	pub metadata: Vec<CommentTableKey>,
	pub type_declarator: TypeDeclarator,
	pub direct_declarators: Vec<DirectDeclarator>,
	pub location: SourceSpan,
}
use crate::analyzer::Variable;
use crate::lexer::NumericConstantTable;
impl VariableDeclarationStatement{
	pub fn create_variable_declaration(&self, already_created: AlreadyCreated, nc_table: &NumericConstantTable, id_table: &IdTable, scope: &mut ModuleImplementationScope) -> miette::Result<Vec<Variable>>{
		if scope.is_generic(){
			return Ok(vec![]);
		}
		let mut kind = VariableKind::from_type_declarator(&self.type_declarator, 0, already_created, nc_table, id_table, scope)?;
		match &kind{
   			VariableKind::Signal(sig) => {
				if sig.is_auto() {
					return Err(miette::Report::new(SemanticError::AutoSpecifierInDeclaration.to_diagnostic_builder().label(self.location, "Auto specifier is not allowed in variable declaration").build()));
				}
				if ! sig.is_direction_specified() {
					return Err(miette::Report::new(SemanticError::MissingDirectionQualifier.to_diagnostic_builder().label(self.location, "Signal must be either input or output").build()));
				}
				if ! sig.is_sensititivity_specified() {
					return Err(miette::Report::new(SemanticError::MissingSensitivityQualifier.to_diagnostic_builder().label(self.location, "Signal must be either const, clock, comb, sync or async").build()));
				}
				if ! sig.is_signedness_specified() {
					return Err(miette::Report::new(SemanticError::MissingSignednessQualifier.to_diagnostic_builder().label(self.location, "Bus signal must be either signed or unsigned").build()));
				}
			},
    		VariableKind::Generic(_) => {
				scope.mark_as_generic();
				return  Ok(vec![]);
			},
			VariableKind::ModuleInstance(_) => unreachable!(""),
		}
		let mut variables = Vec::new();

		for direct_declarator in &self.direct_declarators{
			let mut dimensions = Vec::new();
			for array_declarator in &direct_declarator.array_declarators{
				let size = array_declarator.evaluate(nc_table, 0, scope)?.value;
				if size <= BigInt::from(0){
					return Err(miette::Report::new(SemanticError::NegativeBusWidth.to_diagnostic_builder().label(array_declarator.get_location(), "Array size must be positive").build()));
				}
				dimensions.push(size);
			}
			kind = match kind{
    			VariableKind::Signal(mut sig) => {
					sig.dimensions = dimensions;
					VariableKind::Signal(sig)
				},
    			VariableKind::Generic(mut gen) => {
					gen.dimensions = dimensions;
					VariableKind::Generic(gen)
				},
				VariableKind::ModuleInstance(_) => unreachable!(),
			};
			variables.push(Variable{
				name: direct_declarator.name,
				//dimensions,
				location: direct_declarator.get_location(),
				kind: kind.clone(),
			});
		}
		Ok(variables)
	}
}
impl VariableKind{
	pub fn from_type_declarator(
		type_declarator: &TypeDeclarator, 
		current_scope: usize,
		mut already_created: AlreadyCreated,
		nc_table: &NumericConstantTable,
		id_table: &IdTable,
		scope: &ModuleImplementationScope) -> miette::Result<Self>{
		use TypeSpecifier::*;
		match &type_declarator.specifier{
    		Auto { location } =>{
				already_created = analyze_qualifiers(&type_declarator.qualifiers, already_created, scope, current_scope, id_table)?;
				if already_created.signedness != SignalSignedness::NoSignedness {
					return Ok(VariableKind::Signal(Signal{
						signal_type: SignalType::Bus(BusType{
							width: None,
							signedness: already_created.signedness,
							location: *location,
						}),
						dimensions: Vec::new(),
						sensitivity: already_created.sensitivity,
						direction: already_created.direction,
					}));
				}
				Ok(VariableKind::Signal(Signal { signal_type: SignalType::Auto(location.clone()), dimensions: Vec::new(), sensitivity: already_created.sensitivity, direction: already_created.direction }))
			} 
    		Int { location } => {
				Ok(VariableKind::Generic(GenericVariable { value: None, kind: GenericVariableKind::Int(already_created.signedness, *location), dimensions: Vec::new() }))
			},
    		Wire { location } => {
				already_created = analyze_qualifiers(&type_declarator.qualifiers, already_created, scope, current_scope, id_table)?;
				match already_created.signedness{
        			SignalSignedness::NoSignedness => (),
					_ => return Err(miette::Report::new(SemanticError::ContradictingSpecifier.to_diagnostic_builder().label(*location, "Wire cannot be signed or unsigned").build())),
    			}
				Ok(VariableKind::Signal(Signal { signal_type: SignalType::Wire(*location), sensitivity: already_created.sensitivity, direction: already_created.direction, dimensions: Vec::new() }))
			},
    		Bool { location } => {
				Ok(VariableKind::Generic(GenericVariable { value: None, kind: GenericVariableKind::Bool(*location), dimensions: Vec::new() }))
			},
    		Bus(bus) => {
				already_created = analyze_qualifiers(&type_declarator.qualifiers, already_created, scope, current_scope, id_table)?;
				let width = bus.width.evaluate(nc_table, current_scope, scope)?.value; //FIXME This cannot be evaluated while analyzing declaration of generic module
				if width <= BigInt::from(0) {
					return Err(miette::Report::new(SemanticError::NegativeBusWidth.to_diagnostic_builder().label(bus.location, "Bus width must be positive").build()));
				}
				Ok(VariableKind::Signal(Signal{
					signal_type: SignalType::Bus(BusType{
						width: Some(width),
						signedness: already_created.signedness,
						location: bus.location,
					}),
					dimensions: Vec::new(),
					sensitivity: already_created.sensitivity,
					direction: already_created.direction,
				}))
			},
		}
	}
}


pub fn analyze_qualifiers(qualifiers: &Vec<TypeQualifier>, 
	mut already_created: AlreadyCreated,
	scope: &ModuleImplementationScope,
	current_scope: usize,
	id_table: &IdTable) -> miette::Result<AlreadyCreated> {
	use TypeQualifier::*;
	for qualifier in qualifiers{
		match qualifier{
    		Signed { location } => already_created.add_signedness(SignalSignedness::Signed(*location))?,
    		Unsigned { location } => already_created.add_signedness(SignalSignedness::Unsigned(*location))?,
    		Tristate { location } => already_created.add_direction(Direction::Tristate(*location))?,
    		Const { location } => already_created.add_sensitivity(SignalSensitivity::Const(*location))?,
    		Clock { location } => already_created.add_sensitivity(SignalSensitivity::Clock(*location))?,
			Comb(comb) => {
				let mut sensitivity_list = ClockSensitivityList { list: Vec::new() };
				for signal in &comb.expressions{
					let sensitivity = signal.create_edge_sensitivity()?;
					match scope.get_variable(current_scope, &sensitivity.clock_signal){
    					Some(var) => {
							if ! var.var.is_clock(){
								return Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder().label(sensitivity.location, "This signal is not a clock signal").build()));
							}
						},
    					None => return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder().label(sensitivity.location, "This signal is not declared").build())),
    				}
					sensitivity_list.list.push(sensitivity);
				}
				already_created.add_sensitivity(SignalSensitivity::Comb(sensitivity_list, comb.location))?;
			},
    		Sync(sync) => {
				let mut sensitivity_list = ClockSensitivityList { list: Vec::new() };
				match sync.expressions.len() {
					1 => {
						let name2 = sync.expressions[0].create_edge_sensitivity()?;
						match scope.get_variable(current_scope,&name2.clock_signal) {
								None =>
								return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder()
								.label(name2.location, format!("This variable {:?} is not declared", id_table.get_by_key(&name2.clock_signal).unwrap()).as_str())
								.build())),
								Some(var) => {
									if !var.is_clock(){
										 return  Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder()
										.label(sync.location, "This sync list contains non-clock signals")
										.label(name2.location, format!("This variable {:?} is not a clock signal",id_table.get_by_key(&name2.clock_signal).unwrap()).as_str())
										.build()))
									}
								}
							}
						sensitivity_list.list.push(name2);
					},
					2 => {
						let name2 = sync.expressions[0].create_edge_sensitivity()?;
						let name3 = sync.expressions[1].create_edge_sensitivity()?;
						match scope.get_variable(current_scope,&name2.clock_signal) {
								None =>
								return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder()
								.label(name2.location, format!("This variable {:?} is not declared", id_table.get_by_key(&name2.clock_signal).unwrap()).as_str())
								.build())),
								Some(var) => {
									if !var.is_clock(){
										return  Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder()
										.label(sync.location, "This sync list contains non-clock signals")
										.label(name2.location, format!("This variable {:?} is not a clock signal",id_table.get_by_key(&name2.clock_signal).unwrap()).as_str())
										.build()))
									}
								}
							}
						if name3.clock_signal != name2.clock_signal {
							return Err(miette::Report::new(SemanticError::ForbiddenExpressionInSyncOrComb
									.to_diagnostic_builder()
									.label(sync.location, "\"sync\" qualifier must have at most consists of one variable and its negation")
									.build(),
								));
						}
						if name3.on_rising == name2.on_rising {
							return Err(miette::Report::new(SemanticError::ForbiddenExpressionInSyncOrComb
								.to_diagnostic_builder()
								.label(sync.location, "\"sync\" qualifier must have at most consists of one variable and its negation")
								.build(),
							));
						}
						sensitivity_list.list.push(name2);
						sensitivity_list.list.push(name3);
					},
					_ => return Err(miette::Report::new(
						SemanticError::ForbiddenExpressionInSyncOrComb
							.to_diagnostic_builder()
							.label(
								sync.location,
								"\"sync\" qualifier must have at most consists of one variable and its negation",
							)
							.build(),
					)),
				}
				already_created.add_sensitivity(SignalSensitivity::Sync(sensitivity_list, sync.location))?;
			},
    		Input { location } => already_created.add_direction(Direction::Input(*location))?,
    		Output { location } => already_created.add_direction(Direction::Output(*location))?,
    		Async { location } => already_created.add_sensitivity(SignalSensitivity::Async(*location))?,
    	}
	}
	Ok(already_created)
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct ModuleDeclarationVariableBlock {
	pub metadata: Vec<CommentTableKey>,
	pub types: Vec<TypeQualifier>,
	pub statements: Vec<ModuleDeclarationStatement>,
	pub location: SourceSpan,
}
impl ModuleDeclarationVariableBlock {
	pub fn create_variable_declaration(&self, mut already_created: AlreadyCreated, nc_table: &NumericConstantTable, id_table: &IdTable, declaration_scope: &mut ModuleImplementationScope) -> miette::Result<Vec<Variable>>{
		already_created = analyze_qualifiers(&self.types, already_created, declaration_scope, 0 /*FIXME */, id_table)?;
		let mut variables = Vec::new();
		for statement in &self.statements{
			variables.append(&mut statement.create_variable_declaration(already_created.clone(), nc_table, id_table, declaration_scope)?);
		}
		Ok(variables)
	}
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum ModuleDeclarationStatement {
	VariableDeclarationStatement(VariableDeclarationStatement),
	VariableBlock(ModuleDeclarationVariableBlock),
}

impl ModuleDeclarationStatement {
	pub fn create_variable_declaration(&self, already_created: AlreadyCreated, nc_table: &NumericConstantTable, id_table: &IdTable, scope: &mut ModuleImplementationScope) -> miette::Result<Vec<Variable>>{
		use ModuleDeclarationStatement::*;
		match self{
			VariableDeclarationStatement(declaration) => declaration.create_variable_declaration(already_created, nc_table, id_table, scope),
			VariableBlock(block) => block.create_variable_declaration(already_created, nc_table, id_table, scope),
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
