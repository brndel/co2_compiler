use std::fmt::Display;

use crate::{
    core::Type,
    lexer::{BinaryOperator, UnaryOperator},
    parser::FunctionIdent,
};

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
        name: FunctionIdent<'a>,
        args: Vec<SsaValue<'a>>,
    },
    Allocate {
        target: Option<VirtualRegister<'a>>,
        ty: Type<'a>,
        array_len: Option<SsaValue<'a>>,
    },
    MemGet {
        target: VirtualRegister<'a>,
        source_ptr: SsaValue<'a>,
        offset: usize,
        field_size: usize
    },
    MemSet {
        target_ptr: VirtualRegister<'a>,
        source: SsaValue<'a>,
        offset: usize,
        field_size: usize
    },
    CalcArrayPtr {
        target: VirtualRegister<'a>,
        ptr: SsaValue<'a>,
        index: SsaValue<'a>,
        struct_size: usize,
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
            SsaInstruction::Allocate {
                target,
                ty,
                array_len,
            } => {
                if let Some(target) = target {
                    write!(f, "{} = ", target)?;
                }

                if let Some(arra_len) = array_len {
                    write!(f, "alloc_array({}, {})", ty, arra_len)
                } else {
                    write!(f, "alloc({})", ty)
                }
            }
            SsaInstruction::MemGet {
                target,
                source_ptr: ptr,
                offset,
                field_size: _,
            } => {
                if *offset == 0 {
                    write!(f, "{} = *{}", target, ptr)
                } else {
                    write!(f, "{} = *({:+}){}", target, offset, ptr)
                }
            }
            SsaInstruction::MemSet {
                target_ptr: target,
                source: ptr,
                offset,
                field_size: _,
            } => {
                if *offset == 0 {
                    write!(f, "*{} = {}", target, ptr)
                } else {
                    write!(f, "*({:+}){} = {}", offset, target, ptr)
                }
            }
            SsaInstruction::CalcArrayPtr {
                target,
                ptr,
                index,
                struct_size,
            } => {
                write!(f, "{} = {} + [{} * {}]", target, ptr, index, struct_size)
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
            SsaInstruction::FunctionArg { target, .. } => Some(target),
            SsaInstruction::FunctionCall { target, .. } => target.as_ref(),
            SsaInstruction::Allocate { target, .. } => target.as_ref(),
            SsaInstruction::MemGet { target, .. } => Some(target),
            SsaInstruction::MemSet { target_ptr: target, .. } => Some(target),
            SsaInstruction::CalcArrayPtr { target, .. } => Some(target),
        }
    }
}
