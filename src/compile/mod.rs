mod instruction;
mod codegen;

pub use instruction::Register;
pub use codegen::generate_asm;

use instruction::{Instruction, NumRegister, Value};
use std::path::Path;
use std::{fmt::Write, fs, process::Command};

pub fn compile_code(output: impl AsRef<Path>, instructions: Vec<Instruction>, build_asm: bool) {
    let temp_file_path = output.as_ref().with_extension("s");
    let ouput_file_path = output.as_ref();

    if build_asm {
        let mut assembly = include_str!("template.s").to_string();
    
        for instr in instructions {
            writeln!(&mut assembly, "{}", instr).unwrap();
        }
    
        fs::write(&temp_file_path, assembly).expect("could not write to temp .s file");
    }

    #[cfg(target_arch = "x86_64")]
    {
        let mut gcc = Command::new("gcc")
            .arg(&temp_file_path)
            .arg("-o")
            .arg(&ouput_file_path)
            .spawn()
            .expect("could not start gcc command");

        let gcc_status = gcc.wait().expect("gcc wait failed");
        
        #[cfg(debug_assertions)]
        println!("gcc status: {}", gcc_status);

        #[cfg(debug_assertions)]
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
