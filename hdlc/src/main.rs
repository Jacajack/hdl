extern crate hdllang;

fn main() {
	let s = hdllang::say_something();
	assert_eq!(s, "Hello, i am alive.");
	println!("Hello, world! - {}", s);
}
