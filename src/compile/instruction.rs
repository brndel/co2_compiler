use std::fmt::Display;

use crate::{
    compile::{
        Register,
        register::{FunctionArgRegister, NumRegister64, Register64, StackRegister, SystemRegister},
        value::{Value, Value64},
    },
    ssa::{BlockLabel, SsaValue},
    util::align_bytes,
};

#[derive(Debug)]
pub enum Instruction<'a> {
    Move {
        src: Value,
        dst: Register,
    },
    Move64 {
        src: Value64,
        dst: Register64,
    },
    Add {
        reg: Register,
        value: Value,
    },
    Add64 {
        reg: Register64,
        value: Value64,
    },
    Sub {
        reg: Register,
        value: Value,
    },
    Mul {
        reg: Register,
        value: Value,
    },
    Div {
        reg: Register,
        value: Value,
    },
    Mod {
        reg: Register,
        value: Value,
    },

    BitAnd {
        reg: Register,
        value: Value,
    },
    BitOr {
        reg: Register,
        value: Value,
    },
    BitXor {
        reg: Register,
        value: Value,
    },

    ShiftLeft {
        reg: Register,
        value: Value,
    },
    ShiftRight {
        reg: Register,
        value: Value,
    },

    Negate {
        reg: Register,
    },
    LogicNot {
        reg: Register,
    },
    BitNot {
        reg: Register,
    },

    Compare {
        op: CompareOp,
        target: Register,
        a: Register,
        b: Value,
    },

    // Control
    FunctionProlog {
        max_register: Register,
        params: Vec<Register>,
    },
    Return {
        value: Value,
        max_register: Register,
    },
    Label {
        label: BlockLabel<'a>,
    },
    Jump {
        dst: BlockLabel<'a>,
    },
    JumpConditional {
        condition: Register,
        on_true: BlockLabel<'a>,
        on_false: BlockLabel<'a>,
    },
    CallFunction {
        func: FunctionPointer<'a>,
        dst: Option<Register>,
        params: Vec<Value>,
    },
    Alloc {
        byte_count: usize,
        dst: Option<Register>,
        array_len: Option<Value>,
    },
    MemGet {
        target: Register,
        source_ptr: Value,
        offset: usize,
        field_size: usize,
    },
    MemSet {
        target_ptr: Register,
        source: Value,
        offset: usize,
        field_size: usize,
    },
    CheckArrayLen {
        array_ptr: Value,
        index: Value,
    },
    GlobalLabel {
        label: BlockLabel<'a>,
    },
}

#[derive(Debug)]
pub enum FunctionPointer<'a> {
    User { label: BlockLabel<'a> },
    Builtin(BuiltinFuntion),
}

#[derive(Debug)]
pub enum BuiltinFuntion {
    Print,
    Read,
    Flush,
}

#[derive(Debug)]
pub enum CompareOp {
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equals,
    NotEquals,
}

impl AsRef<str> for CompareOp {
    fn as_ref(&self) -> &str {
        match self {
            CompareOp::Less => "setl",
            CompareOp::LessEq => "setle",
            CompareOp::Greater => "setg",
            CompareOp::GreaterEq => "setge",
            CompareOp::Equals => "sete",
            CompareOp::NotEquals => "setne",
        }
    }
}

impl Display for CompareOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl<'a> Display for Instruction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Move { src, dst } => {
                if src == dst {
                    Ok(())
                } else {
                    match (&src, &dst) {
                        (Value::Register(Register::Stack(_)), Register::Stack(_)) => {
                            writeln!(f, "movl {}, {}", src, Register::Temp)?;
                            write!(f, "movl {}, {}", Register::Temp, dst)
                        }
                        _ => {
                            write!(f, "movl {}, {}", src, dst)
                        }
                    }
                }
            }
            Instruction::Move64 { src, dst } => {
                if src == dst {
                    Ok(())
                } else {
                    match (&src, &dst) {
                        (Value64::Register(Register64::Stack(_)), Register64::Stack(_)) => {
                            writeln!(f, "mov {}, {}", src, Register64::Temp)?;
                            write!(f, "mov {}, {}", Register64::Temp, dst)
                        }
                        _ => {
                            write!(f, "mov {}, {}", src, dst)
                        }
                    }
                }
            }
            Instruction::Add { reg, value } => write!(f, "addl {}, {}", value, reg),
            Instruction::Add64 { reg, value } => write!(f, "add {}, {}", value, reg),
            Instruction::Sub { reg, value } => write!(f, "subl {}, {}", value, reg),
            Instruction::Mul { reg, value } => write!(f, "imul {}, {}", value, reg),
            Instruction::Div { reg, value } => {
                writeln!(f, "movl {}, {}", reg, SystemRegister::Eax)?;
                writeln!(f, "cltd")?;
                match value {
                    value @ Value::Immediate(_) | value @ Value::Register(Register::Stack(_)) => {
                        writeln!(f, "mov {}, {}", value, reg)?;
                        writeln!(f, "idiv {}", reg)?;
                    }
                    Value::Register(register) => writeln!(f, "idiv {}", register)?,
                }
                write!(f, "movl {}, {}", SystemRegister::Eax, reg)
            }
            Instruction::Mod { reg, value } => {
                writeln!(f, "movl {}, {}", reg, SystemRegister::Eax)?;
                writeln!(f, "cltd")?;
                match value {
                    value @ Value::Immediate(_) | value @ Value::Register(Register::Stack(_)) => {
                        writeln!(f, "mov {}, {}", value, reg)?;
                        writeln!(f, "idiv {}", reg)?;
                    }
                    Value::Register(register) => writeln!(f, "idiv {}", register)?,
                }
                write!(f, "movl {}, {}", SystemRegister::Edx, reg)
            }
            Instruction::BitAnd { reg, value } => {
                write!(f, "andl {}, {}", value, reg)
            }
            Instruction::BitOr { reg, value } => {
                write!(f, "orl {}, {}", value, reg)
            }
            Instruction::BitXor { reg, value } => {
                write!(f, "xorl {}, {}", value, reg)
            }
            Instruction::ShiftLeft { reg, value } => {
                writeln!(f, "movl {}, {}", value, SystemRegister::Ecx)?;
                write!(f, "sall %cl, {}", reg)
            }
            Instruction::ShiftRight { reg, value } => {
                writeln!(f, "movl {}, {}", value, SystemRegister::Ecx)?;
                write!(f, "sarl %cl, {}", reg)
            }
            Instruction::Compare { op, target, a, b } => {
                if let &Register::Stack(_) = a {
                    writeln!(f, "mov {}, {}", a, SystemRegister::Eax)?;
                    writeln!(f, "cmp {}, {}", b, SystemRegister::Eax)?;
                } else {
                    writeln!(f, "cmp {}, {}", b, a)?;
                }
                writeln!(f, "{} %al", op)?;
                if let &Register::Stack(_) = target {
                    writeln!(f, "movzbl %al, {}", SystemRegister::Eax)?;
                    write!(f, "mov {}, {}", SystemRegister::Eax, target)
                } else {
                    write!(f, "movzbl %al, {}", target)
                }
            }
            Instruction::Negate { reg } => write!(f, "negl {}", reg),
            Instruction::LogicNot { reg } => write!(f, "xorl {}, {}", Value::Immediate(1), reg),
            Instruction::BitNot { reg } => write!(f, "notl {}", reg),
            Instruction::Label { label } => {
                write!(f, "{}:", label)
            }
            Instruction::Jump { dst } => {
                write!(f, "jmp {}", dst)
            }
            Instruction::JumpConditional {
                condition,
                on_true,
                on_false,
            } => {
                if let &Register::Stack(_) = condition {
                    writeln!(f, "mov {}, {}", condition, SystemRegister::Eax)?;
                    writeln!(f, "test {}, {}", SystemRegister::Eax, SystemRegister::Eax)?;
                } else {
                    writeln!(f, "test {}, {}", condition, condition)?;
                }
                writeln!(f, "jz {}", on_false)?;
                write!(f, "jmp {}", on_true)
            }
            Instruction::FunctionProlog {
                max_register,
                params,
            } => {
                writeln!(f, "push %rbp")?;
                writeln!(f, "mov %rsp, %rbp")?;

                writeln!(f, "push {}", NumRegister64::R12)?;
                writeln!(f, "push {}", NumRegister64::R13)?;
                writeln!(f, "push {}", NumRegister64::R14)?;
                writeln!(f, "push {}", NumRegister64::R15)?;

                if let Register::Stack(StackRegister(registers)) = *max_register {
                    let bytes = align_bytes(registers * 4, 16);

                    writeln!(f, "sub {}, %rsp", Value::Immediate(bytes as i32))?;
                }

                for (idx, target) in params.iter().enumerate() {
                    let param_reg = FunctionArgRegister::get(idx);
                    if let &Register::Stack(_) = target {
                        writeln!(f, "movl {}, {}", param_reg, Register::Temp)?;
                        writeln!(f, "movl {}, {}", Register::Temp, target)?;
                    } else {
                        writeln!(f, "movl {}, {}", param_reg, target)?;
                    }
                }

                Ok(())
            }
            Instruction::Return {
                value,
                max_register,
            } => {
                // Free stack
                if let Register::Stack(StackRegister(registers)) = *max_register {
                    // Align to nearest 16 byte value
                    let bytes = (registers * 4 + 15) & !15;

                    writeln!(f, "add {}, %rsp", Value::Immediate(bytes as i32))?;
                }

                writeln!(f, "mov {}, {}", value, SystemRegister::Eax)?;

                writeln!(f, "pop {}", NumRegister64::R15)?;
                writeln!(f, "pop {}", NumRegister64::R14)?;
                writeln!(f, "pop {}", NumRegister64::R13)?;
                writeln!(f, "pop {}", NumRegister64::R12)?;

                writeln!(f, "leave")?;
                writeln!(f, "ret")
            }
            Instruction::CallFunction { dst, func, params } => {
                writeln!(f, "push {}", Register64::Temp)?;
                writeln!(f, "push {}", NumRegister64::R9)?;
                writeln!(f, "push {}", NumRegister64::R10)?;
                writeln!(f, "push {}", NumRegister64::R11)?;

                match func {
                    FunctionPointer::User { label } => {
                        let param_bytes = (params.len() * 4 + 15) & !15;
                        writeln!(f, "sub {}, %rsp", Value::Immediate(param_bytes as i32))?;

                        for (idx, param) in params.iter().enumerate() {
                            let target = FunctionArgRegister::set(idx);
                            if let &Value::Register(Register::Stack(_)) = param {
                                writeln!(f, "movl {}, {}", param, Register::Temp)?;
                                writeln!(f, "movl {}, {}", Register::Temp, target)?;
                            } else {
                                writeln!(f, "movl {}, {}", param, target)?;
                            }
                        }

                        writeln!(f, "call {}", label)?;

                        writeln!(f, "add {}, %rsp", Value::Immediate(param_bytes as i32))?;
                    }
                    FunctionPointer::Builtin(builtin_funtion) => match builtin_funtion {
                        BuiltinFuntion::Print => {
                            let param = params[0];
                            writeln!(f, "movl {}, %edi", param)?;
                            writeln!(f, "call putchar")?;
                            writeln!(f, "movl {}, {}", Value::Immediate(0), SystemRegister::Eax)?;
                        }
                        BuiltinFuntion::Read => {
                            writeln!(f, "call getchar")?;
                        }
                        BuiltinFuntion::Flush => {
                            writeln!(f, "mov stdout(%rip), %rdi")?;
                            writeln!(f, "call fflush")?;
                            writeln!(f, "movl {}, {}", Value::Immediate(0), SystemRegister::Eax)?;
                        }
                    },
                }

                writeln!(f, "pop {}", NumRegister64::R11)?;
                writeln!(f, "pop {}", NumRegister64::R10)?;
                writeln!(f, "pop {}", NumRegister64::R9)?;
                writeln!(f, "pop {}", Register64::Temp)?;
                if let Some(dst) = dst {
                    writeln!(f, "mov {}, {}", SystemRegister::Eax, dst)?
                }

                Ok(())
            }
            Instruction::Alloc {
                byte_count,
                dst,
                array_len,
            } => {
                writeln!(f, "push {}", Register64::Temp)?;
                writeln!(f, "push {}", NumRegister64::R9)?;
                writeln!(f, "push {}", NumRegister64::R10)?;
                writeln!(f, "push {}", NumRegister64::R11)?;

                if let Some(len) = array_len {
                    let byte_count = byte_count + 8;

                    writeln!(f, "mov {}, %rdi", len)?;
                    writeln!(f, "test %rdi, %rdi")?;
                    writeln!(f, "js call_abort")?;
                    writeln!(f, "mov {}, %rsi", Value::Immediate(byte_count as i32))?;
                    writeln!(f, "call calloc")?;

                    writeln!(f, "mov {}, {}", len, SystemRegister::Eax)?;
                    writeln!(f, "add {}, {}", Value::Immediate(8), SystemRegister::Eax)?;
                } else {
                    writeln!(f, "mov {}, %rdi", Value::Immediate(1))?;
                    writeln!(f, "mov {}, %rsi", Value::Immediate(*byte_count as i32))?;
                    writeln!(f, "call calloc")?;
                }

                writeln!(f, "pop {}", NumRegister64::R11)?;
                writeln!(f, "pop {}", NumRegister64::R10)?;
                writeln!(f, "pop {}", NumRegister64::R9)?;
                writeln!(f, "pop {}", Register64::Temp)?;

                if let Some(dst) = dst {
                    writeln!(f, "mov {}, {}", SystemRegister::Eax, dst)?
                }

                Ok(())
            }
            Instruction::MemGet {
                target,
                source_ptr,
                offset,
                field_size,
            } => {
                if source_ptr.is_stack() {
                    writeln!(f, "movq {} {}", source_ptr, SystemRegister::Eax)?;

                    match field_size {
                        1 => writeln!(f, "movb {}({}), {}", offset, SystemRegister::Eax, target)?,
                        4 => writeln!(f, "movl {}({}), {}", offset, SystemRegister::Eax, target)?,
                        8 => writeln!(
                            f,
                            "movq {}({}), {}",
                            offset,
                            SystemRegister::Eax,
                            target.to_64()
                        )?,
                        _ => unreachable!(),
                    }
                } else {
                    match field_size {
                        1 => writeln!(f, "movb {}({}), {}", offset, source_ptr, target)?,
                        4 => writeln!(f, "movl {}({}), {}", offset, source_ptr, target)?,
                        8 => writeln!(f, "movq {}({}), {}", offset, source_ptr, target.to_64())?,
                        _ => unreachable!(),
                    }
                }

                Ok(())
            }
            Instruction::MemSet {
                target_ptr,
                source,
                offset,
                field_size,
            } => {
                if target_ptr.is_stack() {
                    writeln!(f, "movq {}, {}", target_ptr, SystemRegister::Eax)?;

                    match field_size {
                        1 => writeln!(f, "movb {}, {}({})", source, offset, SystemRegister::Eax)?,
                        4 => writeln!(f, "movl {}, {}({})", source, offset, SystemRegister::Eax)?,
                        8 => writeln!(f, "movq {}, {}({})", source, offset, SystemRegister::Eax,)?,
                        _ => unreachable!(),
                    }
                } else {
                    match field_size {
                        1 => writeln!(f, "movb {}, {}({})", source, offset, target_ptr)?,
                        4 => writeln!(f, "movl {}, {}({})", source, offset, target_ptr)?,
                        8 => writeln!(f, "movq {}, {}({})", source, offset, target_ptr,)?,
                        _ => unreachable!(),
                    }
                }

                Ok(())
            }
            Instruction::CheckArrayLen { array_ptr, index } => {
                if array_ptr.is_stack() {
                    writeln!(f, "movq {}, {}", array_ptr, SystemRegister::Eax)?;
                    writeln!(f, "movq -8({}), {}", SystemRegister::Eax, SystemRegister::Eax)?;
                } else {
                    writeln!(f, "movq -8({}), {}", array_ptr, SystemRegister::Eax)?;
                }

                writeln!(f, "test {}, {}", SystemRegister::Eax, SystemRegister::Eax)?;
                writeln!(f, "js call_abort")?;
                
                writeln!(f, "test {}, {}", SystemRegister::Eax, index)?;
                writeln!(f, "jae call_abort")?;


                Ok(())
            },
            Instruction::GlobalLabel { label } => write!(f, ".global {}", label),
        }
    }
}
