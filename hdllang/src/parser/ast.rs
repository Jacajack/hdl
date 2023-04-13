mod expression;
mod opcodes;
mod module_declaration_statement;
mod match_expression;
mod type_qualifier;
mod type_specifier;
mod type_declarator;
mod type_name;
mod direct_declarator;
mod direct_initializer;
mod variable_definition;
mod variable_declaration;
mod port_bind_statement;
mod module_implementation_statement;
mod variable_block;
mod top_definition;
mod root;
pub use root::*;
pub use top_definition::*;
pub use variable_block::*;
pub use module_implementation_statement::*;
pub use port_bind_statement::*;
pub use variable_declaration::*;
pub use variable_definition::*;
pub use direct_initializer::*;
pub use direct_declarator::*;
pub use type_name::*;
pub use type_declarator::*;
pub use type_qualifier::*;
pub use type_specifier::*;
pub use match_expression::*;
pub use module_declaration_statement::*;
pub use expression::*;
pub use opcodes::*;
use crate::SourceSpan;

pub trait SourceLocation{
    fn get_location(&self)->SourceSpan;
}