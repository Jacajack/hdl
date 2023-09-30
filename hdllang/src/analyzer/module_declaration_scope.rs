//use log::debug;
//use num_bigint::BigInt;
//use num_traits::Signed;
//use std::collections::HashMap;

//use super::{CombinedQualifiers, SemanticError, VariableDeclared};
//use crate::lexer::IdTableKey;
//use crate::{ProvidesCompilerDiagnostic, SourceSpan};
//#[derive(Debug, Clone)]
//pub struct ModuleDeclarationScope {
//	pub variables: HashMap<IdTableKey, (VariableDeclared, SourceSpan)>,
//	pub bus_widths: HashMap<IdTableKey, BigInt>,
//	pub array_sizes: HashMap<IdTableKey, Vec<BigInt>>,
//}
//impl ModuleDeclarationScope {
//	pub fn new() -> Self {
//		ModuleDeclarationScope {
//			variables: HashMap::new(),
//			bus_widths: HashMap::new(),
//			array_sizes: HashMap::new(),
//		}
//	}
//	pub fn is_declared(&self, name: &IdTableKey) -> Option<SourceSpan> {
//		self.variables.get(name).map(|(_, span)| *span)
//	}
//	pub fn declare(&mut self, name: IdTableKey, variable: VariableDeclared, span: SourceSpan) {
//		self.variables.insert(name, (variable, span));
//	}
//	pub fn get_variable(&self, name: &IdTableKey) -> Option<&(VariableDeclared, SourceSpan)> {
//		self.variables.get(name)
//	}
//	pub fn is_generic(&self) -> bool {
//		for (var, _loc) in self.variables.values() {
//			match var.specifier {
//				crate::parser::ast::TypeSpecifier::Int { .. } | crate::parser::ast::TypeSpecifier::Bool { .. } => {
//					return true
//				},
//				_ => continue,
//			};
//		}
//		false
//	}
//	pub fn analyze(
//		&mut self,
//		id_table: &crate::lexer::IdTable,
//		nc_table: &crate::lexer::NumericConstantTable,
//	) -> miette::Result<()> {
//		for name in self.variables.keys() {
//			let (variable, span) = self.variables.get(name).unwrap();
//			match &variable.qualifiers {
//				CombinedQualifiers {
//					synchronous: Some((exprs, location)),
//					..
//				} => {
//					match exprs.len() {
//						1 => {
//							let name2 = exprs[0].get_name_sync_or_comb()?;
//							match self.variables.get(&name2) {
//									None =>
//									return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder()
//									.label(*location, format!("This variable {:?} is not declared", id_table.get_by_key(&name2).unwrap()).as_str())
//									.label(*span, format!("This variable's {:?} qualifier \"sync\" references a variable that is not declared", id_table.get_by_key(name).unwrap()).as_str())
//									.build())),
//									Some((var2,loc)) => {
//										match &var2.qualifiers{
//											CombinedQualifiers{clock: Some(_), ..} => (),
//											_ => return  Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder()
//											.label(*span, format!("This variable's {:?} sync list contains non-clock signals",id_table.get_by_key(name).unwrap()).as_str())
//											.label(*location, "This is the sync list")
//											.label(*loc, format!("This variable {:?} is not a clock signal",id_table.get_by_key(&name2).unwrap()).as_str())
//											.build()))
//										}
//									}
//								}
//						},
//						2 => {
//							let name2 = exprs[0].get_name_sync_or_comb()?;
//							let name3 = exprs[1].get_name_sync_or_comb()?;
//							match self.variables.get(&name2) {
//									None =>
//									return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder()
//									.label(*location, format!("This variable {:?} is not declared", id_table.get_by_key(&name2).unwrap()).as_str())
//									.label(*span, format!("This variable's {:?} qualifier \"sync\" references a variable that is not declared", id_table.get_by_key(name).unwrap()).as_str())
//									.build())),
//									Some((var2,loc)) => {
//										match &var2.qualifiers{
//											CombinedQualifiers{clock: Some(_), ..} => (),
//											_ => return  Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder()
//											.label(*span, format!("This variable's {:?} sync list contains non-clock signals",id_table.get_by_key(name).unwrap()).as_str())
//											.label(*location, "This is the sync list")
//											.label(*loc, format!("This variable {:?} is not a clock signal",id_table.get_by_key(&name2).unwrap()).as_str())
//											.build()))
//										}
//									}
//								}
//							if name3 != name2 {
//								return Err(miette::Report::new(SemanticError::ForbiddenExpressionInSyncOrComb
//										.to_diagnostic_builder()
//										.label(*span, format!("This variable's {:?} sync list contains two different variables",id_table.get_by_key(name).unwrap()).as_str())
//										.label(*location, "\"sync\" qualifier must have at most consists of one variable and its negation")
//										.build(),
//									));
//							}
//						},
//						_ => return Err(miette::Report::new(
//							SemanticError::ForbiddenExpressionInSyncOrComb
//								.to_diagnostic_builder()
//								.label(
//									*span,
//									format!(
//										"This variable's {:?} sync list contains more than two variables",
//										id_table.get_by_key(name).unwrap()
//									)
//									.as_str(),
//								)
//								.label(
//									*location,
//									"\"sync\" qualifier must have at most consists of one variable and its negation",
//								)
//								.build(),
//						)),
//					}
//				},
//				CombinedQualifiers {
//					comb: Some((expressions, span1)),
//					..
//				} => {
//					for expr in expressions {
//						let name2 = expr.get_name_sync_or_comb().map_err(|e| e)?;
//						if name2 == *name {
//							return Err(miette::Report::new(
//								SemanticError::VariableReferencingItself
//									.to_diagnostic_builder()
//									.label(
//										*span,
//										format!(
//											"This variable's {:?} comb list contains itself",
//											id_table.get_by_key(name).unwrap()
//										)
//										.as_str(),
//									)
//									.label(*span1, "This is the comb list")
//									.build(),
//							));
//						}
//						match self.variables.get(&name2) {
//								None =>
//								return Err(miette::Report::new(SemanticError::VariableNotDeclared.to_diagnostic_builder()
//								.label(*span1, format!("This variable {:?} is not declared", id_table.get_by_key(&name2).unwrap()).as_str())
//								.label(*span, format!("This variable's {:?} qualifier \"comb\" references a variable that is not declared", id_table.get_by_key(name).unwrap()).as_str())
//								.build())),
//								Some((var2,loc)) =>{
//									match &var2.qualifiers{
//										CombinedQualifiers{clock: Some(_), ..} => (),
//										_ => return  Err(miette::Report::new(SemanticError::NotClockSignalInSync.to_diagnostic_builder()
//										.label(*span, format!("This variable's {:?} comb list contains non-clock signals",id_table.get_by_key(name).unwrap()).as_str())
//										.label(*span1, "This is the comb list")
//										.label(*loc, format!("This variable {:?} is not a clock signal",id_table.get_by_key(&name2).unwrap()).as_str())
//										.build()))
//									}
//								}
//							}
//					}
//				},
//				_ => (),
//			}
//			use crate::parser::ast::TypeSpecifier::*;
//			match &variable.specifier {
//				Bus(bus) => {
//					let val = bus.width.evaluate_in_declaration(nc_table)?;
//					debug!("Bus width is {}", val.value);
//					if val.value.is_negative() {
//						return Err(miette::Report::new(
//							SemanticError::NegativeBusWidth
//								.to_diagnostic_builder()
//								.label(bus.location, "Bus width cannot be negative")
//								.build(),
//						));
//					}
//					self.bus_widths.insert(variable.name, val.value);
//				},
//				_ => (),
//			};
//			let mut compiled_array = Vec::new();
//			for expr in &variable.array {
//				let val = expr.evaluate_in_declaration(nc_table)?;
//				debug!("Array size is {}", val.value);
//				if val.value.is_negative() {
//					return Err(miette::Report::new(
//						SemanticError::NegativeBusWidth
//							.to_diagnostic_builder()
//							.label(
//								crate::parser::ast::SourceLocation::get_location(expr),
//								"Array size cannot be negative",
//							)
//							.build(),
//					));
//				}
//				compiled_array.push(val.value);
//			}
//			self.array_sizes.insert(variable.name, compiled_array);
//		}
//		Ok(())
//	}
//}
