use crate::lexer::{Operator, Spanned, UnaryOperator};

#[derive(Debug, Clone)]
pub enum Statement<'a, Num = ValueNum> {
    Declaration {
        ident: Spanned<&'a str>,
        value: Option<Expression<'a, Num>>,
    },
    Assignment {
        ident: Spanned<&'a str>,
        op: Option<Operator>,
        value: Expression<'a, Num>,
    },
    Return {
        expr: Expression<'a, Num>
    }
}

#[derive(Debug, Clone)]
pub enum Expression<'a, Num = ValueNum> {
    Ident(Spanned<&'a str>),
    Num(Num),
    // DecNum(Spanned<&'a str>),
    // HexNum(Spanned<&'a str>),
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


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseNum<'a> {
    Dec(Spanned<&'a str>),
    Hex(Spanned<&'a str>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValueNum(pub Spanned<i32>);

impl ValueNum {
    pub fn num(&self) -> i32 {
        self.0.0
    }
}