pub trait HasComment {
	fn get_comment(&self) -> Option<String>;
}