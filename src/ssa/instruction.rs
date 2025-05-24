use std::fmt::Display;

use crate::lexer::{BinaryOperator, Operator, UnaryOperator};

use super::register::VirtualRegister;

#[derive(Debug, Clone, Copy)]
pub enum SsaInstruction<'a> {
    Move {
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
    Return {
        value: SsaValue<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
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

impl<'a> SsaInstruction<'a> {

    pub fn target(&self) -> Option<&VirtualRegister<'a>> {
        match self {
            SsaInstruction::Move { target, .. } => Some(target),
            SsaInstruction::BinaryOp { target, .. } => Some(target),
            SsaInstruction::UnaryOp { target, .. } => Some(target),
            SsaInstruction::Return { .. } => None,
        }
    }

}