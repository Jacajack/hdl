use crate::core::{numeric_constant_table::NumericConstantTable, CommentTable, IdTable};
use crate::parser::ast::Root;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct SerializerContext {
	pub ast_root: Root,
	pub id_table: IdTable,
	pub comment_table: CommentTable,
	pub nc_table: NumericConstantTable,
}
