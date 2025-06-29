mod codegen;
mod instruction;
mod register;
mod value;

pub use codegen::generate_asm;
pub use register::Register;

use instruction::Instruction;
use std::path::Path;
use std::{fmt::Write, fs};

pub fn compile_code(output: impl AsRef<Path>, instructions: Vec<Instruction>, build_asm: bool) {
    let temp_file_path = output.as_ref().with_extension("s");

    if build_asm {
        let mut assembly = include_str!("template.s").to_string();

        for instr in instructions {
            writeln!(&mut assembly, "{}", instr).unwrap();
        }

        fs::write(&temp_file_path, assembly).expect("could not write to temp .s file");
    } else {
        println!(
            "NOT GENERATING ASSEMBLY FILE, USING {} - build_asm is false",
            temp_file_path.display()
        );
    }

    #[cfg(target_arch = "x86_64")]
    {
        use std::process::Command;

        let ouput_file_path = output.as_ref();

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
        {
            // Only execute the program in debug mode
            let status = Command::new(ouput_file_path)
                .output()
                .expect("failed to run binary");

            println!("binary result: {}", status.status);
            println!("binary stdout: {}", String::from_utf8_lossy(&status.stdout));
        }
    }

    #[cfg(not(target_arch = "x86_64"))]
    {
        println!("can't compile code on non x86_64 machines")
    }
}
