use super::Expression;
use crate::core::IdTableKey;
#[derive(serde::Serialize, serde::Deserialize, Clone, Eq, PartialEq)]
pub struct PostfixWithArgs {
	pub id: IdTableKey,
	pub argument_list: Vec<Expression>,
	pub location: crate::SourceSpan,
}


/*
int foo(){};
int main(){
	********foo;
	*foo;
	***foo;
}


*/