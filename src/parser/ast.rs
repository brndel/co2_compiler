use chumsky::span::SimpleSpan;

use crate::{
    core::Type,
    lexer::{AssignOperator, BinaryOperator, GetSpan, Spanned, UnaryOperator},
};

#[derive(Debug, Clone)]
pub struct Block<'a, Num = ValueNum> {
    pub statements: Vec<Statement<'a, Num>>,
}

#[derive(Debug, Clone)]
pub enum Statement<'a, Num = ValueNum> {
    Declaration {
        ty: Spanned<Type<'a>>,
        ident: Spanned<&'a str>,
        value: Option<Expression<'a, Num>>,
    },
    Assignment {
        lvalue: Lvalue<'a, Num>,
        op: Option<AssignOperator>,
        value: Expression<'a, Num>,
    },
    FunctionCall(FunctionCall<'a, Num>),
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
        value: Expression<'a, Num>,
    },
    Break(SimpleSpan),
    Continue(SimpleSpan),
    Block(Block<'a, Num>),
}


#[derive(Debug, Clone)]
pub enum Lvalue<'a, Num = ValueNum> {
    Ident(Spanned<&'a str>),
    Ptr {
        lvalue: Box<Self>,
        ptr: Ptr<'a, Num>
    }
}

impl<'a, Num> From<(Lvalue<'a, Num>, Ptr<'a, Num>)> for Lvalue<'a, Num> {
    fn from(value: (Lvalue<'a, Num>, Ptr<'a, Num>)) -> Self {
        Self::Ptr { lvalue: Box::new(value.0), ptr: value.1 }
    }
}


#[derive(Debug, Clone)]
pub enum Expression<'a, Num = ValueNum> {
    Ident(Spanned<&'a str>),
    Num(Num),
    Bool(Spanned<bool>),
    NullPtr(SimpleSpan),
    Binary {
        a: Box<Self>,
        op: Spanned<BinaryOperator>,
        b: Box<Self>,
    },
    Unary {
        op: Spanned<UnaryOperator>,
        expr: Box<Self>,
    },
    Ternary {
        condition: Box<Self>,
        a: Box<Self>,
        b: Box<Self>,
    },
    FunctionCall(FunctionCall<'a, Num>),
    Access {
        expr: Box<Self>,
        ptr: Ptr<'a, Num>
    },
}

impl<'a, Num> From<(Expression<'a, Num>, Ptr<'a, Num>)> for Expression<'a, Num> {
    fn from(value: (Expression<'a, Num>, Ptr<'a, Num>)) -> Self {
        Self::Access { expr: Box::new(value.0), ptr: value.1 }
    }
}

#[derive(Debug, Clone)]
pub enum Ptr<'a, Num = ValueNum> {
    /// a.b
    FieldAccess { ident: Spanned<&'a str> },
    /// a->b
    PtrFieldAccess { ident: Spanned<&'a str> },
    /// a[b]
    ArrayAccess { index: Box<Expression<'a, Num>> },
    /// *a
    PtrDeref,
}

#[derive(Debug, Clone)]
pub enum FunctionCall<'a, Num = ValueNum> {
    Alloc {
        ty: Spanned<Type<'a>>,
    },
    AllocArray {
        ty: Spanned<Type<'a>>,
        len: Box<Expression<'a, Num>>,
    },
    Fn {
        ident: Spanned<FunctionIdent<'a>>,
        args: Vec<Expression<'a, Num>>,
    },
}

#[derive(Debug, Clone)]
pub enum FunctionIdent<'a> {
    User(&'a str),
    Print,
    Read,
    Flush,
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
