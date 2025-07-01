use crate::{
    core::Type,
    lexer::Spanned,
    parser::{Block, ValueNum},
};

#[derive(Debug, Clone)]
pub struct Program<'a, Num = ValueNum> {
    pub defs: Vec<TopLevelDef<'a, Num>>,
}

#[derive(Debug, Clone)]
pub enum TopLevelDef<'a, Num = ValueNum> {
    Function(FunctionDef<'a, Num>),
    Struct(StructDef<'a>),
}

#[derive(Debug, Clone)]
pub struct FunctionDef<'a, Num = ValueNum> {
    pub return_type: Spanned<Type<'a>>,
    pub ident: Spanned<&'a str>,
    pub params: Vec<FunctionParam<'a>>,
    pub block: Block<'a, Num>,
}

#[derive(Debug, Clone)]
pub struct FunctionParam<'a> {
    pub ty: Spanned<Type<'a>>,
    pub name: Spanned<&'a str>,
}

#[derive(Debug, Clone)]
pub struct StructDef<'a> {
    pub ident: Spanned<&'a str>,
    pub fields: Vec<StructField<'a>>,
}

#[derive(Debug, Clone)]
pub struct StructField<'a> {
    pub ty: Spanned<Type<'a>>,
    pub ident: Spanned<&'a str>,
}
