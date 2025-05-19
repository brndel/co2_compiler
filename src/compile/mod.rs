mod instruction;

use std::path::Path;

#[cfg(target_arch = "x86_64")]
pub fn compile_code(output: impl AsRef<Path>) {
    use instruction::{Instruction, NumRegister, Register, Value};
    use std::{fmt::Write, fs, process::Command};

    let input_file_path = output.as_ref().with_extension("s");
    let ouput_file_path = output.as_ref().with_extension("bin");

    let mut assembly = include_str!("template.s").to_string();

    let instructions = vec![
        Instruction::Move {
            src: Value::Immediate(77),
            dst: NumRegister::R8.into(),
        },
        Instruction::Move {
            src: Value::Immediate(10),
            dst: NumRegister::R9.into(),
        },
        Instruction::Mod {
            register: NumRegister::R8.into(),
            value: NumRegister::R9.into(),
        },
        Instruction::Return {
            register: NumRegister::R8.into(),
        },
    ];

    for instr in instructions {
        writeln!(&mut assembly, "{}", instr).unwrap();
    }

    fs::write(&input_file_path, assembly).expect("could not write to temp .s file");

    println!("STARTING GCC");

    let mut gcc = Command::new("gcc")
        .arg(input_file_path)
        .arg("-o")
        .arg(&ouput_file_path)
        .spawn()
        .expect("could not start gcc command");

    gcc.wait().expect("gcc wait failed");
    println!("DONE");

    let status = Command::new(ouput_file_path).output().expect("failed to run binary");

    println!("binary result: {}", status.status);
}

#[cfg(not(target_arch = "x86_64"))]
pub fn compile_code(output: impl AsRef<Path>) {
    panic!("can't compile code on non x86_64 machines")
}
