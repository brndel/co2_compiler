use std::{fmt::Display, str::FromStr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    Separator(Separator),
    Ident(&'a str),
    DecNum(&'a str),
    HexNum(&'a str),
    Keyword(Keyword),
    Operator(Operator),
    Assign(Option<Operator>),
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Separator {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Mod => write!(f, "%"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
}


impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Minus => write!(f, "-"),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Struct,
    If,
    Else,
    While,
    For,
    Continue,
    Break,
    Return,
    Assert,
    True,
    False,
    Null,
    Print,
    Read,
    Alloc,
    AllocArray,
    Int,
    Bool,
    Void,
    Char,
    String,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "struct" => Ok(Keyword::Struct),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "while" => Ok(Keyword::While),
            "for" => Ok(Keyword::For),
            "continue" => Ok(Keyword::Continue),
            "break" => Ok(Keyword::Break),
            "return" => Ok(Keyword::Return),
            "assert" => Ok(Keyword::Assert),
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "NULL" => Ok(Keyword::Null),
            "print" => Ok(Keyword::Print),
            "read" => Ok(Keyword::Read),
            "alloc" => Ok(Keyword::Alloc),
            "alloc_array" => Ok(Keyword::AllocArray),
            "int" => Ok(Keyword::Int),
            "bool" => Ok(Keyword::Bool),
            "void" => Ok(Keyword::Void),
            "char" => Ok(Keyword::Char),
            "string" => Ok(Keyword::String),
            _ => Err(()),
        }
    }
}
