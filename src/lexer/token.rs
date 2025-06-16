use std::{fmt::Display, str::FromStr};

use super::{AssignOperator, Operator};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    Separator(Separator),
    Ident(&'a str),
    DecNum(&'a str),
    HexNum(&'a str),
    Keyword(Keyword),
    Operator(Operator),
    Assign(Option<AssignOperator>),
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
    Flush,
    Alloc,
    AllocArray,
    Int,
    Bool,
    Void,
    Char,
    String,
}

impl AsRef<str> for Keyword {
    fn as_ref(&self) -> &str {
        match self {
            Keyword::Struct => "struct",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::For => "for",
            Keyword::Continue => "continue",
            Keyword::Break => "break",
            Keyword::Return => "return",
            Keyword::Assert => "assert",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Null => "NULL",
            Keyword::Print => "print",
            Keyword::Read => "read",
            Keyword::Flush => "flush",
            Keyword::Alloc => "alloc",
            Keyword::AllocArray => "alloc_array",
            Keyword::Int => "int",
            Keyword::Bool => "bool",
            Keyword::Void => "void",
            Keyword::Char => "char",
            Keyword::String => "string",
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
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
            "flush" => Ok(Keyword::Flush),
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
