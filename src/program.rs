use crate::{lexer::Spanned, parser::Statement};


#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub main_fn_span: Spanned<&'a str>,
    pub statements: Vec<Statement<'a>>,
}
