use crate::{
    program::Program,
    semantic::{SemanticError, namespace::StructNamespace},
};

pub fn get_struct_namespace<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    program: &Program<'src, Num>,
) -> StructNamespace<'src> {
    let mut namespace = StructNamespace::new(program);

    for def in program.structs() {
        if let Err(err) = namespace.calculate_size_of(def.ident) {
            errors.push(err);
        }
    }

    namespace.finish()
}
