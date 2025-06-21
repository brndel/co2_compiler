use std::fmt::Display;

use crate::lexer::{BinaryOperator, UnaryOperator};

use super::register::VirtualRegister;

#[derive(Debug, Clone)]
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
    FunctionArg {
        index: usize,
        target: VirtualRegister<'a>,
    },
    FunctionCall {
        target: Option<VirtualRegister<'a>>,
        name: &'a str,
        args: Vec<SsaValue<'a>>,
    }
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
            SsaInstruction::FunctionArg { index, target } => {
                write!(f, "{} = arg({})", target, index)
            }
            SsaInstruction::FunctionCall { target, name, args } => {
                if let Some(target) = target {
                    write!(f, "{} = ", target)?;
                }
                write!(f, "{}(", name)?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl<'a> SsaInstruction<'a> {
    pub fn target(&self) -> Option<&VirtualRegister<'a>> {
        match self {
            SsaInstruction::Move { target, .. } => Some(target),
            SsaInstruction::PhiMove { target, .. } => Some(target),
            SsaInstruction::BinaryOp { target, .. } => Some(target),
            SsaInstruction::UnaryOp { target, .. } => Some(target),
            SsaInstruction::FunctionArg { index, target, .. } => Some(target),
            SsaInstruction::FunctionCall { target, .. } => target.as_ref(),
        }
    }
}
