use std::fmt::Display;

use chumsky::{Parser, error::Rich, extra, prelude::just, span::SimpleSpan};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    // Int math
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    // Bool logic
    LogicNot,
    LogicAnd,
    LogicOr,
    // Bitwise logic
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    // Bitshift
    ShiftLeft,
    ShiftRight,
    // Compare
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equals,
    NotEquals,
    // Ternary
    TernaryQuestionMark,
    TernaryColon,
    // Arrow
    Arrow,
}

impl Operator {
    pub fn parser<'src>()
    -> impl Parser<'src, &'src str, Operator, extra::Err<Rich<'src, char, SimpleSpan>>> + Clone
    {
        just("->")
            .to(Operator::Arrow)
            // Int math
            .or(just("+").to(Operator::Plus))
            .or(just("-").to(Operator::Minus))
            .or(just("*").to(Operator::Mul))
            .or(just("/").to(Operator::Div))
            .or(just("%").to(Operator::Mod))
            // -insertion- parse != before !
            .or(just("!=").to(Operator::NotEquals))
            // Bool logic
            .or(just("!").to(Operator::LogicNot))
            .or(just("&&").to(Operator::LogicAnd))
            .or(just("||").to(Operator::LogicOr))
            // Bitwise logic
            .or(just("~").to(Operator::BitNot))
            .or(just("&").to(Operator::BitAnd))
            .or(just("|").to(Operator::BitOr))
            .or(just("^").to(Operator::BitXor))
            // Bitshift
            .or(just("<<").to(Operator::ShiftLeft))
            .or(just(">>").to(Operator::ShiftRight))
            // Compare
            .or(just("<=").to(Operator::LessEq))
            .or(just("<").to(Operator::Less))
            .or(just(">=").to(Operator::GreaterEq))
            .or(just(">").to(Operator::Greater))
            .or(just("==").to(Operator::Equals))
            // Ternary
            .or(just("?").to(Operator::TernaryQuestionMark))
            .or(just(":").to(Operator::TernaryColon))
    }
}

impl AsRef<str> for Operator {
    fn as_ref(&self) -> &str {
        match self {
            // Int math
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Mod => "%",
            // Bool logic
            Operator::LogicNot => "!",
            Operator::LogicAnd => "&&",
            Operator::LogicOr => "||",
            // Bitwise logic
            Operator::BitNot => "~",
            Operator::BitAnd => "&",
            Operator::BitOr => "|",
            Operator::BitXor => "^",
            // Bitshift
            Operator::ShiftLeft => "<<",
            Operator::ShiftRight => ">>",
            // Compare
            Operator::Less => "<",
            Operator::LessEq => "<=",
            Operator::Greater => ">",
            Operator::GreaterEq => ">=",
            Operator::Equals => "==",
            Operator::NotEquals => "!=",
            // Ternary
            Operator::TernaryQuestionMark => "?",
            Operator::TernaryColon => ":",
            // Arrow
            Operator::Arrow => "->",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    // Int math
    Minus,
    // Bool logic
    LogicNot,
    // Bitwise logic
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Int math
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    // Bool logic
    LogicAnd,
    LogicOr,
    // Bitwise logic
    BitAnd,
    BitOr,
    BitXor,
    // Bitshift
    ShiftLeft,
    ShiftRight,
    // Compare
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equals,
    NotEquals,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOperator {
    // Int math
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    // Bitwise logic
    BitAnd,
    BitOr,
    BitXor,
    // Bitshift
    ShiftLeft,
    ShiftRight,
}

impl From<UnaryOperator> for Operator {
    fn from(value: UnaryOperator) -> Self {
        match value {
            UnaryOperator::Minus => Self::Minus,
            UnaryOperator::LogicNot => Self::LogicNot,
            UnaryOperator::BitNot => Self::BitNot,
        }
    }
}

impl TryFrom<Operator> for UnaryOperator {
    type Error = ();

    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        Ok(match value {
            Operator::Minus => Self::Minus,
            Operator::LogicNot => Self::LogicNot,
            Operator::BitNot => Self::BitNot,
            _ => return Err(()),
        })
    }
}

impl From<BinaryOperator> for Operator {
    fn from(value: BinaryOperator) -> Self {
        match value {
            BinaryOperator::Plus => Self::Plus,
            BinaryOperator::Minus => Self::Minus,
            BinaryOperator::Mul => Self::Mul,
            BinaryOperator::Div => Self::Div,
            BinaryOperator::Mod => Self::Mod,
            BinaryOperator::LogicAnd => Self::LogicAnd,
            BinaryOperator::LogicOr => Self::LogicOr,
            BinaryOperator::BitAnd => Self::BitAnd,
            BinaryOperator::BitOr => Self::BitOr,
            BinaryOperator::BitXor => Self::BitXor,
            BinaryOperator::ShiftLeft => Self::ShiftLeft,
            BinaryOperator::ShiftRight => Self::ShiftRight,
            BinaryOperator::Less => Self::Less,
            BinaryOperator::LessEq => Self::LessEq,
            BinaryOperator::Greater => Self::Greater,
            BinaryOperator::GreaterEq => Self::GreaterEq,
            BinaryOperator::Equals => Self::Equals,
            BinaryOperator::NotEquals => Self::NotEquals,
        }
    }
}

impl TryFrom<Operator> for BinaryOperator {
    type Error = ();

    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        Ok(match value {
            Operator::Plus => Self::Plus,
            Operator::Minus => Self::Minus,
            Operator::Mul => Self::Mul,
            Operator::Div => Self::Div,
            Operator::Mod => Self::Mod,
            Operator::LogicAnd => Self::LogicAnd,
            Operator::LogicOr => Self::LogicOr,
            Operator::BitAnd => Self::BitAnd,
            Operator::BitOr => Self::BitOr,
            Operator::BitXor => Self::BitXor,
            Operator::ShiftLeft => Self::ShiftLeft,
            Operator::ShiftRight => Self::ShiftRight,
            Operator::Less => Self::Less,
            Operator::LessEq => Self::LessEq,
            Operator::Greater => Self::Greater,
            Operator::GreaterEq => Self::GreaterEq,
            Operator::Equals => Self::Equals,
            Operator::NotEquals => Self::NotEquals,
            _ => return Err(()),
        })
    }
}

impl From<AssignOperator> for Operator {
    fn from(value: AssignOperator) -> Self {
        match value {
            AssignOperator::Plus => Self::Plus,
            AssignOperator::Minus => Self::Minus,
            AssignOperator::Mul => Self::Mul,
            AssignOperator::Div => Self::Div,
            AssignOperator::Mod => Self::Mod,
            AssignOperator::BitAnd => Self::BitAnd,
            AssignOperator::BitOr => Self::BitOr,
            AssignOperator::BitXor => Self::BitXor,
            AssignOperator::ShiftLeft => Self::ShiftLeft,
            AssignOperator::ShiftRight => Self::ShiftRight,
        }
    }
}

impl TryFrom<Operator> for AssignOperator {
    type Error = ();

    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        Ok(match value {
            Operator::Plus => Self::Plus,
            Operator::Minus => Self::Minus,
            Operator::Mul => Self::Mul,
            Operator::Div => Self::Div,
            Operator::Mod => Self::Mod,
            Operator::BitAnd => Self::BitAnd,
            Operator::BitOr => Self::BitOr,
            Operator::BitXor => Self::BitXor,
            Operator::ShiftLeft => Self::ShiftLeft,
            Operator::ShiftRight => Self::ShiftRight,
            _ => return Err(()),
        })
    }
}

impl From<AssignOperator> for BinaryOperator {
    fn from(value: AssignOperator) -> Self {
        match value {
            AssignOperator::Plus => Self::Plus,
            AssignOperator::Minus => Self::Minus,
            AssignOperator::Mul => Self::Mul,
            AssignOperator::Div => Self::Div,
            AssignOperator::Mod => Self::Mod,
            AssignOperator::BitAnd => Self::BitAnd,
            AssignOperator::BitOr => Self::BitOr,
            AssignOperator::BitXor => Self::BitXor,
            AssignOperator::ShiftLeft => Self::ShiftLeft,
            AssignOperator::ShiftRight => Self::ShiftRight,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op: Operator = (*self).into();
        write!(f, "{}", op)
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op: Operator = (*self).into();
        write!(f, "{}", op)
    }
}

impl Display for AssignOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op: Operator = (*self).into();
        write!(f, "{}", op)
    }
}
