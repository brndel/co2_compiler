use std::fmt::Display;

use crate::lexer::{Operator, UnaryOperator};

use super::register::VirtualRegister;

pub enum SsaInstruction<'a> {
    Move {
        target: VirtualRegister<'a>,
        source: SsaValue<'a>,
    },
    BinaryOp {
        target: VirtualRegister<'a>,
        a: SsaValue<'a>,
        op: Operator,
        b: SsaValue<'a>,
    },
    UnaryOp {
        target: VirtualRegister<'a>,
        op: UnaryOperator,
        value: VirtualRegister<'a>,
    },
    Return {
        value: SsaValue<'a>,
    },
}

pub enum SsaValue<'a> {
    Register(VirtualRegister<'a>),
    Immediate(i32),
}

impl<'a> Display for SsaValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SsaValue::Register(virtual_register) => write!(f, "{}", virtual_register),
            SsaValue::Immediate(value) => write!(f, "[{}]", value),
        }
    }
}

impl<'a> Display for SsaInstruction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SsaInstruction::Move { target, source } => write!(f, "{} = {}", target, source),
            SsaInstruction::BinaryOp { target, a, op, b } => write!(f, "{} = {} {} {}", target, a, op, b),
            SsaInstruction::UnaryOp { target, op, value } => {
                write!(f, "{} = {} {}", target, op, value)
            }
            SsaInstruction::Return { value } => write!(f, "return {}", value),
        }
    }
}
