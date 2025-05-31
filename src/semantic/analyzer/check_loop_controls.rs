use crate::{
    lexer::Keyword,
    parser::{Block, Statement},
    program::Program,
    semantic::SemanticError,
};

pub fn check_loop_controls<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    program: &Program<'a, Num>,
) {
    validate_block(errors, &program.block, false);
}

fn validate_block<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    block: &Block<'a, Num>,
    inside_loop: bool,
) {
    for statement in &block.statements {
        validate_statement(errors, statement, inside_loop);
    }
}

fn validate_statement<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    statement: &Statement<'a, Num>,
    inside_loop: bool,
) {
    match &statement {
        Statement::Declaration { .. } | Statement::Assignment { .. } | Statement::Return { .. } => {
            ()
        }
        Statement::If {
            condition: _,
            then,
            r#else,
        } => {
            validate_statement(errors, then, inside_loop);
            if let Some(r#else) = r#else {
                validate_statement(errors, r#else, inside_loop);
            }
        }
        Statement::While { condition: _, body: then } => {
            validate_statement(errors, then, true);
        }
        Statement::For {
            init,
            condition: _,
            step,
            body: then,
        } => {
            if let Some(init) = init {
                validate_statement(errors, init, inside_loop);
            }
            if let Some(step) = step {
                match step.as_ref() {
                    Statement::Assignment { .. } => (),
                    Statement::Declaration { ident, .. } => {
                        errors.push(SemanticError::DeclareInForLoopStep {
                            span: ident.1,
                        });
                    }
                    _ => {
                        panic!("invalid statement in for loop step")
                    }
                }
                validate_statement(errors, step, inside_loop);
            }
            validate_statement(errors, then, true);
        }
        Statement::Break(span) => {
            if !inside_loop {
                errors.push(SemanticError::LoopControlsOutsideLoop {
                    keyword: (Keyword::Break, *span),
                });
            }
        }
        Statement::Continue(span) => {
            if !inside_loop {
                errors.push(SemanticError::LoopControlsOutsideLoop {
                    keyword: (Keyword::Continue, *span),
                });
            }
        }
        Statement::Block(block) => validate_block(errors, block, inside_loop),
    }
}
