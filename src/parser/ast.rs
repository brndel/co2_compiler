use crate::lexer::{Operator, Spanned, UnaryOperator};

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Declaration {
        ident: Spanned<&'a str>,
        value: Option<Expression<'a>>,
    },
    Assignment {
        ident: Spanned<&'a str>,
        op: Option<Operator>,
        value: Expression<'a>,
    },
    Return {
        expr: Expression<'a>
    }
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Ident(Spanned<&'a str>),
    DecNum(Spanned<&'a str>),
    HexNum(Spanned<&'a str>),
    Binary {
        a: Box<Self>,
        op: Operator,
        b: Box<Self>,
    },
    Unary {
        op: UnaryOperator,
        expr: Box<Self>,
    },
}
