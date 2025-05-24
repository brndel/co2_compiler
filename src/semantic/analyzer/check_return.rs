use crate::{parser::Statement, program::Program, semantic::SemanticError};

pub fn check_return<'a, Num>(errors: &mut Vec<SemanticError<'a>>, program: &Program<'a, Num>) {
    let does_return = program.block.statements.iter().any(does_return);

    if !does_return {
        errors.push(SemanticError::NoReturnInFunction {
            ident: program.main_fn_span,
        });
    }
}

pub fn does_return<'a, Num>(statement: &Statement<'a, Num>) -> bool {
    match statement {
        Statement::Declaration { .. } => false,
        Statement::Assignment { .. } => false,
        Statement::If {
            condition: _,
            then,
            r#else,
        } => does_return(then) && r#else.as_ref().is_none_or(|r#else| does_return(r#else)),
        Statement::While { .. } => false,
        Statement::For { .. } => false,
        Statement::Return { .. } => true,
        Statement::Break(_) => false,
        Statement::Continue(_) => false,
        Statement::Block(block) => block.statements.iter().any(does_return),
    }
}
