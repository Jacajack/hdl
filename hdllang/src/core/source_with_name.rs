#[derive(Debug, Clone)]
pub struct SourceWithName{
	name: String,
	source: String,
}
impl SourceWithName{
	pub fn new(name: String, source: String)->Self{
		Self{
			name,
			source,
		}
	}
	pub fn into_named_source(self) -> miette::NamedSource{
		miette::NamedSource::new( self.name, self.source)
	}
}