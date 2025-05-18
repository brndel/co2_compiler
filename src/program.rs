use crate::{lexer::Spanned, parser::{Statement, ValueNum}};


#[derive(Debug, Clone)]
pub struct Program<'a, Num = ValueNum> {
    pub main_fn_span: Spanned<&'a str>,
    pub statements: Vec<Statement<'a, Num>>,
}
