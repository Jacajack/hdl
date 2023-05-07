use crate::parser::ast::{Expression,SourceLocation,opcodes::*};
use crate::core::SourceSpan;
use serde::{Serialize,Deserialize};
#[derive(Serialize,Deserialize,Debug)]
pub struct RangeExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub code: RangeOpcode,
    pub location: SourceSpan,
}
impl SourceLocation for RangeExpression {
    fn get_location(&self) -> SourceSpan {
        self.location
    }
}