use crate::{
    program::Program,
    semantic::{
        SemanticError,
        namespace::FunctionNamespace,
    },
};

pub fn get_func_namespace<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    program: &Program<'src, Num>,
) -> FunctionNamespace<'src> {
    let mut namespace = FunctionNamespace::new();

    for func in program.functions() {
        namespace.add_function(func, errors);
    }

    namespace
}
