mod direct_declarator;
mod direct_initializer;
mod expression;
mod match_expression;
mod module_declaration_statement;
mod module_implementation_statement;
mod opcodes;
mod port_bind_statement;
mod root;
mod top_definition;
mod type_declarator;
mod type_name;
mod type_qualifier;
mod type_specifier;
mod variable_block;
mod variable_declaration;
mod variable_definition;
use crate::SourceSpan;
pub use direct_declarator::*;
pub use direct_initializer::*;
pub use expression::*;
pub use match_expression::*;
pub use module_declaration_statement::*;
pub use module_implementation_statement::*;
pub use opcodes::*;
pub use port_bind_statement::*;
pub use root::*;
pub use top_definition::*;
pub use type_declarator::*;
pub use type_name::*;
pub use type_qualifier::*;
pub use type_specifier::*;
pub use variable_block::*;
pub use variable_declaration::*;
pub use variable_definition::*;

pub trait SourceLocation {
	fn get_location(&self) -> SourceSpan;
}
