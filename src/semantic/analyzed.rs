use crate::{
    parser::ParseNum,
    program::Program, semantic::namespace::StructNamespace,
};

use super::{analyzer, SemanticError};

pub struct Analyzed<'a> {
    pub program: Program<'a>,
    pub structs: StructNamespace<'a>,
}

impl<'a> Analyzed<'a> {
    pub fn new(program: Program<'a, ParseNum<'a>>) -> Result<Self, Vec<SemanticError<'a>>> {
        let mut errors = Vec::new();

        let structs = analyzer::get_struct_namespace(&mut errors, &program);
        let functions = analyzer::get_func_namespace(&mut errors, &program);

        analyzer::check_main_fn(&mut errors, &program);
        analyzer::check_status_and_types(&mut errors, &program, &functions, &structs);
        analyzer::check_loop_controls(&mut errors, &program);
        analyzer::check_return(&mut errors, &program);
        let program = analyzer::map_number(&mut errors, program);

        if let Some(program) = program && errors.is_empty() {
            Ok(Self {
                program,
                structs,
            })
        } else {
            Err(errors)
        }
    }
}
