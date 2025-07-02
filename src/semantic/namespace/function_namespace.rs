use std::{
    borrow::Cow,
    collections::HashMap,
    sync::LazyLock,
};

use chumsky::span::SimpleSpan;

use crate::{
    core::Type,
    lexer::Spanned,
    parser::{FunctionCall, FunctionIdent},
    program::{FunctionDef, FunctionParam},
    semantic::SemanticError,
};

pub struct FunctionNamespace<'src> {
    functions: HashMap<FunctionIdent<'src>, FunctionInfo<'src>>,
}

#[derive(Clone)]
pub enum FunctionInfo<'src> {
    Alloc(Type<'src>),
    AllocArray(Type<'src>),
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
    pub fn return_type(&self) -> Type<'src> {
        match self {
            FunctionInfo::Print => Type::Int,
            FunctionInfo::Read => Type::Int,
            FunctionInfo::Flush => Type::Int,
            FunctionInfo::Alloc(ty) => Type::Pointer(Box::new(ty.clone())),
            FunctionInfo::AllocArray(ty) => Type::Array(Box::new(ty.clone())),
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
            FunctionInfo::Alloc(_) => &[],
            FunctionInfo::AllocArray(_) => {
                // SimpleSpan::splat is not const
                static PARAMS: LazyLock<[FunctionParam<'static>; 1]> = LazyLock::new(|| {
                    [FunctionParam {
                        ty: (Type::Int, SimpleSpan::splat(0)),
                        name: ("len", SimpleSpan::splat(0)),
                    }]
                });
                PARAMS.as_ref()
            }
        }
    }
}

impl<'src> FunctionNamespace<'src> {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        functions.insert(FunctionIdent::Print, FunctionInfo::Print);
        functions.insert(FunctionIdent::Read, FunctionInfo::Read);
        functions.insert(FunctionIdent::Flush, FunctionInfo::Flush);

        Self { functions }
    }

    pub fn add_function<Num>(
        &mut self,
        func: &FunctionDef<'src, Num>,
        errors: &mut Vec<SemanticError<'src>>,
    ) {
        for param in &func.params {
            if param.ty.0.is_big_type() {
                errors.push(SemanticError::DisallowedBigType {
                    ty: param.ty.clone(),
                });
            }
        }

        if func.return_type.0.is_big_type() {
            errors.push(SemanticError::DisallowedBigType {
                ty: func.return_type.clone(),
            });
        }

        if let Some(FunctionInfo::User { ident, .. }) =
            self.functions.get(&FunctionIdent::User(func.ident.0))
        {
            errors.push(SemanticError::FunctionAlreadyDefined {
                ident: func.ident,
                defined_at: ident.1,
            })
        } else {
            self.functions.insert(
                FunctionIdent::User(func.ident.0),
                FunctionInfo::User {
                    return_type: func.return_type.clone(),
                    ident: func.ident,
                    params: func.params.clone(),
                },
            );
        }
    }

    pub fn get_function<Num>(
        &self,
        fn_call: &FunctionCall<'src, Num>,
    ) -> Result<Cow<FunctionInfo<'src>>, SemanticError<'src>> {
        match fn_call {
            FunctionCall::Alloc { ty, .. } => Ok(Cow::Owned(FunctionInfo::Alloc(ty.0.clone()))),
            FunctionCall::AllocArray { ty, .. } => {
                Ok(Cow::Owned(FunctionInfo::AllocArray(ty.0.clone())))
            }
            FunctionCall::Fn { ident, .. } => {
                let info =
                    self.functions
                        .get(&ident.0)
                        .ok_or(SemanticError::FunctionNotDefined {
                            call_ident: ident.clone(),
                        })?;

                Ok(Cow::Borrowed(info))
            }
        }
    }
}
