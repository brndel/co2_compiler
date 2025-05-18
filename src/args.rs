use std::{env, process::exit};

#[derive(Debug, Clone)]
pub struct CompilerArgs {
    pub input_file: String,
    pub output_file: String,
}

pub fn get_args() -> CompilerArgs {
    match get_args_opt() {
        Some(args) => args,
        None => {
            eprintln!("Invalid arguments: Expected one input file and one output file");
            exit(3)
        }
    }
}

fn get_args_opt() -> Option<CompilerArgs> {
    let mut args = env::args().skip(1);

    let input_file = args.next()?;
    let output_file = args.next()?;

    if args.next().is_some() {
        return None;
    }

    Some(CompilerArgs {
        input_file,
        output_file,
    })
}
