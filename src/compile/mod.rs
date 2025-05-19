mod instruction;
mod codegen;

pub use instruction::Register;
pub use codegen::generate_asm;

use instruction::{Instruction, NumRegister, Value};
use std::path::Path;
use std::{fmt::Write, fs, process::Command};

pub fn compile_code(output: impl AsRef<Path>, instructions: Vec<Instruction>) {
    let input_file_path = output.as_ref().with_extension("s");
    let ouput_file_path = output.as_ref().with_extension("bin");

    let mut assembly = include_str!("template.s").to_string();

    for instr in instructions {
        writeln!(&mut assembly, "{}", instr).unwrap();
    }

    fs::write(&input_file_path, assembly).expect("could not write to temp .s file");

    #[cfg(target_arch = "x86_64")]
    {
        println!("starting gcc");

        let mut gcc = Command::new("gcc")
            .arg(input_file_path)
            .arg("-o")
            .arg(&ouput_file_path)
            .spawn()
            .expect("could not start gcc command");

        gcc.wait().expect("gcc wait failed");
        println!("gcc done");

        // #[cfg(debug_assertions)]
        { // Only execute the program in debug mode
            let status = Command::new(ouput_file_path)
                .output()
                .expect("failed to run binary");
    
            println!("binary result: {}", status.status);
        }

    }

    #[cfg(not(target_arch = "x86_64"))]
    {
        println!("can't compile code on non x86_64 machines")
    }
}
