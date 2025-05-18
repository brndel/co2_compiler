use std::{fs, path::Path, process::Command};

#[cfg(target_arch = "x86_64")]
pub fn compile_code(output: impl AsRef<Path>) {
    let input_file_path = output.as_ref().with_extension("s");
    let ouput_file_path = output.as_ref().with_extension("bin");

    let assembly = include_str!("template.s").to_string();

    fs::write(&input_file_path, assembly).unwrap();

    println!("STARTING GCC");

    let mut gcc = Command::new("gcc")
        .arg(input_file_path)
        .arg("-o")
        .arg(ouput_file_path)
        .arg("-nostdlib")
        .arg("-static")
        .spawn()
        .unwrap();

    gcc.wait().unwrap();
    println!("DONE");
}

#[cfg(not(target_arch = "x86_64"))]
pub fn compile_code(output: impl AsRef<Path>) {
    panic!("can't compile code on non x86_64 machines")
}

