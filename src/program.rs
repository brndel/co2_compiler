use crate::{core::Type, lexer::Spanned, parser::{Block, ValueNum}};


#[derive(Debug, Clone)]
pub struct Program<'a, Num = ValueNum> {
    pub functions: Vec<Function<'a, Num>>,
}

#[derive(Debug, Clone)]
pub struct Function<'a, Num = ValueNum> {
    pub return_type: Spanned<Type>,
    pub ident: Spanned<&'a str>,
    pub params: Vec<FunctionParam<'a>>,
    pub block: Block<'a, Num>
}

#[derive(Debug, Clone)]
pub struct FunctionParam<'a> {
    pub ty: Spanned<Type>,
    pub name: Spanned<&'a str>,
}