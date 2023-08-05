pub struct Container<'a, T>
	where T: ContainerItem<'a, T>
{
	items: Vec<T>,
}

impl<'a, T> Container<'a, T> {
	pub fn new() -> Self {
		Self {
			items: Vec::new(),
		}
	}

	pub fn new_item(&mut self) -> ContainerRef<'a, T> {
		let item = T::new_in_container(self);
		self.items.push(item);
		ContainerRef {
			cont: self,
			id: self.items.len() - 1,
		}
	}

	pub fn get_by_ref(&self, r: ContainerRef<T>) -> Option<&T> {
		self.items.get(r.id)
	}
}

pub struct ContainerRef<'a, T> {
	cont: &'a Container<'a, T>,
	id: usize,
}

pub trait ContainerItem<T> {
	fn new_in_container(cont: &Container<'a, T>) -> T;
	fn get_container(&self) -> &Container<'a, T>;
	fn get_container_ref(&self) -> ContainerRef<'a, T> {
		ContainerRef {
			cont: self.get_container(),
			id: 0,
		}
	}
}

impl<'a, T> std::ops::Deref for ContainerRef<'a, T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		self.cont.get_by_ref(*self).unwrap()
	}
}

#[cfg(test)]
mod test {
	use super::*;

	struct TestType<'a> {
		container: &'a Container<Self>,
		pub s: String,
		
	}

	impl<'a> ContainerItem<'a, TestType<'a>> for TestType<'a> {
		fn new_in_container(cont: &Container<'a, T>) -> T {
			TestType {
				container: cont,
				s: String::new(),
			}
		}
		
		fn get_container(&self) -> &Container<'a, Self> {
			self.container
		}
	}

	fn basic_test() {
		let mut c = Container::<TestType>::new();
	}
}