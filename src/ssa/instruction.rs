use std::fmt::Display;

use crate::lexer::{BinaryOperator, UnaryOperator};

use super::register::VirtualRegister;

#[derive(Debug, Clone, Copy)]
pub enum SsaInstruction<'a> {
    Move {
        target: VirtualRegister<'a>,
        source: SsaValue<'a>,
    },
    PhiMove {
        target: VirtualRegister<'a>,
        source: SsaValue<'a>,
    },
    BinaryOp {
        target: VirtualRegister<'a>,
        a: SsaValue<'a>,
        op: BinaryOperator,
        b: SsaValue<'a>,
    },
    UnaryOp {
        target: VirtualRegister<'a>,
        op: UnaryOperator,
        value: VirtualRegister<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum SsaValue<'a> {
    Register(VirtualRegister<'a>),
    ImmediateNum(i32),
    ImmediateBool(bool),
}

impl<'a> Display for SsaValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SsaValue::Register(virtual_register) => write!(f, "{}", virtual_register),
            SsaValue::ImmediateNum(value) => write!(f, "[{}]", value),
            SsaValue::ImmediateBool(value) => write!(f, "[{}]", value),
        }
    }
}

impl<'a> Display for SsaInstruction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SsaInstruction::Move { target, source } => write!(f, "{} = {}", target, source),
            SsaInstruction::PhiMove { target, source } => write!(f, "{} '= {}", target, source),
            SsaInstruction::BinaryOp { target, a, op, b } => {
                write!(f, "{} = {} {} {}", target, a, op, b)
            }
            SsaInstruction::UnaryOp { target, op, value } => {
                write!(f, "{} = {} {}", target, op, value)
            }
        }
    }
}

impl<'a> SsaInstruction<'a> {
    pub fn target(&self) -> &VirtualRegister<'a> {
        match self {
            SsaInstruction::Move { target, .. } => target,
            SsaInstruction::PhiMove { target, .. } => target,
            SsaInstruction::BinaryOp { target, .. } => target,
            SsaInstruction::UnaryOp { target, .. } => target,
        }
    }
}
