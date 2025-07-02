use std::{collections::BTreeMap, sync::LazyLock};

use chumsky::span::SimpleSpan;

use crate::{core::Type, lexer::Spanned, parser::FunctionCall, program::{FunctionDef, FunctionParam, Program}, semantic::SemanticError};


pub struct FunctionNamespace<'src> {
    functions: BTreeMap<&'src str, FunctionInfo<'src>>,
}

pub enum FunctionInfo<'src> {
    Print,
    Read,
    Flush,
    User {
        return_type: Spanned<Type<'src>>,
        ident: Spanned<&'src str>,
        params: Vec<FunctionParam<'src>>,
    },
}

impl<'src> FunctionInfo<'src> {
    pub fn return_type(&self) -> Type {
        match self {
            FunctionInfo::Print => Type::Int,
            FunctionInfo::Read => Type::Int,
            FunctionInfo::Flush => Type::Int,
            FunctionInfo::User { return_type, .. } => return_type.0.clone(),
        }
    }

    pub fn params(&self) -> &[FunctionParam<'src>] {
        match self {
            FunctionInfo::Print => {
                // SimpleSpan::splat is not const
                static PARAMS: LazyLock<[FunctionParam<'static>; 1]> = LazyLock::new(|| {
                    [FunctionParam {
                        ty: (Type::Int, SimpleSpan::splat(0)),
                        name: ("name", SimpleSpan::splat(0)),
                    }]
                });
                PARAMS.as_ref()
            }
            FunctionInfo::Read => &[],
            FunctionInfo::Flush => &[],
            FunctionInfo::User { params, .. } => &params,
        }
    }
}

impl<'src> FunctionNamespace<'src> {
    pub fn new<Num>(program: &Program<'src, Num>, errors: &mut Vec<SemanticError<'src>>) -> Self {
        let mut functions = BTreeMap::new();

        functions.insert("print", FunctionInfo::Print);
        functions.insert("read", FunctionInfo::Read);
        functions.insert("flush", FunctionInfo::Flush);

        let mut this = Self { functions };

        todo!()
        // for func in &program.functions {
        //     match this.add_function(func) {
        //         Ok(()) => (),
        //         Err(err) => errors.push(err),
        //     }
        // }

        // this
    }

    fn add_function<Num>(&mut self, func: &FunctionDef<'src, Num>) -> Result<(), SemanticError<'src>> {
        todo!()
        // if let Some(FunctionInfo::User { ident, .. }) = self.functions.get(func.ident.0) {
        //     Err(SemanticError::FunctionAlreadyDefined {
        //         ident: func.ident,
        //         defined_at: ident.1,
        //     })
        // } else {
        //     self.functions.insert(
        //         func.ident.0,
        //         FunctionInfo::User {
        //             return_type: func.return_type,
        //             ident: func.ident,
        //             params: func.params.clone(),
        //         },
        //     );
        //     Ok(())
        // }
    }

    pub fn get_function<Num>(
        &self,
        fn_call: &FunctionCall<'src, Num>,
    ) -> Result<&FunctionInfo<'src>, SemanticError<'src>> {
        todo!()
        // self.functions
        //     .get(fn_call.ident.0)
        //     .ok_or(SemanticError::FunctionNotDefined {
        //         call_ident: fn_call.ident,
        //     })
    }
}
