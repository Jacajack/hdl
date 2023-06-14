use crate::parser::ast::Root;
use crate::core::{IdTable, CommentTable,numeric_constant_table::NumericConstantTable};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub struct SerializerContext{
    pub ast_root: Root,
    pub id_table: IdTable,
    pub comment_table: CommentTable,
    pub nc_table: NumericConstantTable,
}