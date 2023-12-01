use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
	static ref VALID_ID_REGEX: Regex = Regex::new(r"^[a-zA-Z_][0-9a-zA-Z_]*$").unwrap();
}

/// Checks if given name is valid for HIRN
pub(super) fn is_name_valid(name: &str) -> bool {
	// FIXME this is likely bad for perf
	VALID_ID_REGEX.is_match(name)
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_valid_names() {
		assert!(is_name_valid("test"));
		assert!(is_name_valid("test222"));
		assert!(is_name_valid("test222iiasdia____7"));
		assert!(is_name_valid("_lorem__ipsum_22_33_whatever"));
	}

	#[test]
	fn test_invalid_names() {
		assert!(!is_name_valid("$$fff"));
		assert!(!is_name_valid("1horse"));
		assert!(!is_name_valid("hor!se"));
		assert!(!is_name_valid("hor se"));
	}
}
