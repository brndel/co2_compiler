use crate::{lexer::Spanned, parser::{Block, Statement, ValueNum}};


#[derive(Debug, Clone)]
pub struct Program<'a, Num = ValueNum> {
    pub main_fn_span: Spanned<&'a str>,
    pub block: Block<'a, Num>
}