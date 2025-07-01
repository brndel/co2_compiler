use crate::{parser::Statement, program::Program, semantic::SemanticError};

pub fn check_return<'a, Num>(errors: &mut Vec<SemanticError<'a>>, program: &Program<'a, Num>) {

    todo!()

    // for func in &program.functions {
    //     let does_return = func.block.statements.iter().any(does_return);
    
    //     if !does_return {
    //         errors.push(SemanticError::NoReturnInFunction {
    //             ident: func.ident,
    //         });
    //     }
    // }
}

pub fn does_return<'a, Num>(statement: &Statement<'a, Num>) -> bool {
    match statement {
        Statement::Declaration { .. } => false,
        Statement::Assignment { .. } => false,
        Statement::If {
            condition: _,
            then,
            r#else,
        } => does_return(then) && r#else.as_ref().is_some_and(|r#else| does_return(r#else)),
        Statement::While { .. } => false,
        Statement::For { .. } => false,
        Statement::Return { .. } => true,
        Statement::Break(_) => false,
        Statement::Continue(_) => false,
        Statement::Block(block) => block.statements.iter().any(does_return),
        Statement::FunctionCall(_) => false,
    }
}
