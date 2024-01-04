extern crate hdllang;
use rstest::*;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use subprocess::{ExitStatus, Popen, PopenConfig};
use tempfile::NamedTempFile;

// Copied from main.rs
// It would be nice if we had this living in hdllang crate.
fn compile(code: String, file_name: String, output: &mut dyn Write, elab: bool) -> miette::Result<()> {
	hdllang::utils::elaborate(code, file_name, output, elab, false)
}

fn pretty_print(code: String, output: &mut dyn Write) -> miette::Result<()> {
	hdllang::utils::pretty_print(code, output)
}

fn run_hdlc(input_path: &Path, elab: bool) -> miette::Result<NamedTempFile> {
	let src = std::fs::read_to_string(input_path).expect("failed to read source code");
	let mut tmpfile = NamedTempFile::new().unwrap();
	compile(
		src,
		input_path
			.file_name()
			.expect("filename needed")
			.to_str()
			.unwrap()
			.into(),
		&mut tmpfile,
		elab,
	)?;
	Ok(tmpfile)
}

fn pretty_print_file(input_path: &Path) -> miette::Result<NamedTempFile> {
	let src = std::fs::read_to_string(input_path).expect("failed to read source code");
	let mut tmpfile = NamedTempFile::new().unwrap();
	pretty_print(src, &mut tmpfile)?;
	Ok(tmpfile)
}

fn run_iverilog(iverilog_path: &Path, input_paths: &Vec<PathBuf>) -> Result<NamedTempFile, String> {
	let bin_file = NamedTempFile::new().unwrap();

	let mut args = vec![
		iverilog_path.to_str().unwrap(),
		"-g2005-sv",
		"-Wall",
		"-I",
		"tests/include",
		"-o",
		bin_file.path().to_str().unwrap(),
	];
	args.extend(input_paths.iter().map(|p| p.to_str().unwrap()));

	let mut p = Popen::create(&args, PopenConfig::default()).expect("failed to spawn iverilog");

	use ExitStatus::*;
	match p.wait().unwrap() {
		Exited(0) => {},
		Exited(err) => return Err(format!("Exit code: {}", err)),
		Signaled(signum) => return Err(format!("Signaled: {}", signum)),
		Other(err) => return Err(format!("Other: {}", err)),
		Undetermined => panic!("cannot determine exit code"),
	}

	Ok(bin_file)
}

fn compile_run_iverilog_with_sim(input_path: &Path) -> Result<(), String> {
	let iverilog_path = std::env::var("IVERILOG_PATH").unwrap_or("iverilog".into());
	let vvp_path = std::env::var("VVP_PATH").unwrap_or("vvp".into());

	let dump_file = NamedTempFile::new().unwrap();
	let compiled_file = run_hdlc(input_path, true).expect("compile failed");
	let tb_file = input_path.with_extension("sv");

	let input_files = vec![compiled_file.path().to_path_buf(), tb_file];
	let bin_file = run_iverilog(Path::new(&iverilog_path), &input_files).expect("iverilog failed");

	let mut p = Popen::create(
		&[
			vvp_path.as_str(),
			"-n",
			bin_file.path().to_str().unwrap(),
			format!("+DUMP_PATH={}", dump_file.path().to_str().unwrap()).as_str(),
		],
		PopenConfig::default(),
	)
	.expect("testbench run failed");

	match p.wait().unwrap() {
		ExitStatus::Exited(0) => {},
		other => panic!("testbench run failed: {:?}", other),
	}

	Ok(())
}

fn compile_run_iverilog(path: PathBuf) {
	let sv_file = run_hdlc(path.as_path(), true).unwrap();

	if !std::env::var("NO_IVERILOG").is_ok() {
		let iverilog_path = std::env::var("IVERILOG_PATH").unwrap_or("iverilog".into());
		match run_iverilog(Path::new(&iverilog_path), &vec![sv_file.path().to_path_buf()]) {
			Ok(_) => {},
			Err(e) => {
				eprintln!("iverilog failed: {}", e);
				eprintln!("Faulty SV codegen follows:");
				eprintln!("============================");
				eprintln!("{}", std::fs::read_to_string(sv_file.path()).unwrap());
				eprintln!("============================");
				panic!("iverilog failed");
			},
		}
	}
}

fn pretty_print_and_compile_run_iverilog(path: PathBuf) {
	let sv_file_orignal = run_hdlc(path.as_path(), false).unwrap();
	let pretty_printed = pretty_print_file(path.as_path()).unwrap();
	let sv_file = run_hdlc(pretty_printed.path(), false).unwrap();
	let mut org_str = String::new();
	sv_file_orignal.as_file().read_to_string(&mut org_str).unwrap();
	let mut pretty_printed_str = String::new();
	sv_file.as_file().read_to_string(&mut pretty_printed_str).unwrap();
	assert_eq!(org_str, pretty_printed_str);
}

#[rstest]
fn test_compile_success(#[files("tests/input/*.hirl")] path: PathBuf) {
	compile_run_iverilog(path)
}

#[rstest]
fn test_compile_success_sim_files(#[files("tests/input_sim/*.hirl")] path: PathBuf) {
	compile_run_iverilog(path)
}

#[rstest]
fn test_compile_failure(#[files("tests/input_invalid/*.hirl")] path: PathBuf) {
	assert!(run_hdlc(path.as_path(), false).is_err());
}

#[rstest]
fn test_elab_failure(#[files("tests/input_elab_invalid/*.hirl")] path: PathBuf) {
	assert!(run_hdlc(path.as_path(), true).is_err());
}

#[rstest]
fn test_compile_simulate(#[files("tests/input_sim/*.hirl")] path: PathBuf) {
	assert!(compile_run_iverilog_with_sim(&path).is_ok());
}

#[rstest]
fn test_pretty_print_sim_files(#[files("tests/input_sim/*.hirl")] path: PathBuf) {
	pretty_print_and_compile_run_iverilog(path)
}
