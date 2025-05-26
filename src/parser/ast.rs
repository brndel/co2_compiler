use std::fmt::Display;

use chumsky::span::SimpleSpan;

use crate::lexer::{AssignOperator, BinaryOperator, GetSpan, Spanned, UnaryOperator};

#[derive(Debug, Clone)]
pub struct Block<'a, Num = ValueNum> {
    pub statements: Vec<Statement<'a, Num>>,
}

#[derive(Debug, Clone)]
pub enum Statement<'a, Num = ValueNum> {
    Declaration {
        ty: Spanned<Type>,
        ident: Spanned<&'a str>,
        value: Option<Expression<'a, Num>>,
    },
    Assignment {
        ident: Spanned<&'a str>,
        op: Option<AssignOperator>,
        value: Expression<'a, Num>,
    },
    If {
        condition: Expression<'a, Num>,
        then: Box<Self>,
        r#else: Option<Box<Self>>,
    },
    While {
        condition: Expression<'a, Num>,
        body: Box<Self>,
    },
    For {
        init: Option<Box<Self>>,
        condition: Expression<'a, Num>,
        step: Option<Box<Self>>,
        body: Box<Self>,
    },
    Return {
        expr: Expression<'a, Num>,
    },
    Break(SimpleSpan),
    Continue(SimpleSpan),
    Block(Block<'a, Num>),
}

#[derive(Debug, Clone)]
pub enum Expression<'a, Num = ValueNum> {
    Ident(Spanned<&'a str>),
    Num(Num),
    Bool(Spanned<bool>),
    Binary {
        a: Box<Self>,
        op: BinaryOperator,
        b: Box<Self>,
    },
    Unary {
        op: UnaryOperator,
        expr: Box<Self>,
    },
    Ternary {
        condition: Box<Self>,
        a: Box<Self>,
        b: Box<Self>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
        }
    }
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


impl<'a> GetSpan for ParseNum<'a> {
    fn span(&self) -> chumsky::prelude::SimpleSpan {
        match self {
            ParseNum::Dec(value) => value.span(),
            ParseNum::Hex(value) => value.span(),
        }
    }
}

impl GetSpan for ValueNum {
    fn span(&self) -> chumsky::prelude::SimpleSpan {
        self.0.span()
    }
}