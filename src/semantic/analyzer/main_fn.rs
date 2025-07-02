use crate::{core::Type, lexer::GetSpan, program::Program, semantic::SemanticError};

pub fn check_main_fn<'src, Num>(errors: &mut Vec<SemanticError<'src>>, program: &Program<'src, Num>)
where
    Num: GetSpan,
{
    let mut main_funcs = program
        .functions()
        .filter(|func| func.ident.0 == "main");

    let Some(main) = main_funcs.next() else {
        errors.push(SemanticError::NoMainFunction);
        return;
    };

    if main.return_type.0 != Type::Int {
        errors.push(SemanticError::MissmatchedType {
            ty: main.return_type.clone(),
            expected_type: Type::Int,
        });
    }

    if !main.params.is_empty() {
        errors.push(SemanticError::MainFnWithParams { ident: main.ident });
    }
}
