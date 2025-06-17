use crate::{
    parser::ParseNum,
    program::Program,
};

use super::{analyzer, SemanticError};

pub struct Analyzed<'a> {
    pub program: Program<'a>,
}

impl<'a> Analyzed<'a> {
    pub fn new(program: Program<'a, ParseNum<'a>>) -> Result<Self, Vec<SemanticError<'a>>> {
        let mut errors = Vec::new();

        analyzer::check_main_fn(&mut errors, &program);
        analyzer::check_status_and_types(&mut errors, &program);
        analyzer::check_loop_controls(&mut errors, &program);
        analyzer::check_return(&mut errors, &program);
        let program = analyzer::map_number(&mut errors, program);

        match program {
            Some(program) if errors.is_empty() => Ok(Self { program }),
            _ => Err(errors),
        }
    }
}
