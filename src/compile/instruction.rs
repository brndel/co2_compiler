use std::{cell::Ref, fmt::Display};

use crate::{register_alloc::GraphColor, ssa::{BlockLabel, VirtualRegister}};

#[derive(Debug)]
pub enum Instruction<'a> {
    Move {
        src: Value,
        dst: Register,
    },
    Add {
        reg: Register,
        value: Value,
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
        label: BlockLabel<'a>,
        dst: Option<Register>,
        params: Vec<Value>,
    },
    GlobalLabel {
        label: BlockLabel<'a>,
    },
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
            Instruction::Move { src, dst } => match (&src, &dst) {
                (Value::Register(Register::Stack(_)), Register::Stack(_)) => {
                    writeln!(f, "movl {}, {}", src, Register::Temp)?;
                    write!(f, "movl {}, {}", Register::Temp, dst)
                }
                _ => {
                    write!(f, "movl {}, {}", src, dst)
                }
            },
            Instruction::Add { reg, value } => write!(f, "add {}, {}", value, reg),
            Instruction::Sub { reg, value } => write!(f, "sub {}, {}", value, reg),
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
            Instruction::Negate { reg } => write!(f, "neg {}", reg),
            Instruction::LogicNot { reg } => write!(f, "xor {}, {}", Value::Immediate(1), reg),
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
            Instruction::FunctionProlog { max_register, params } => {
                writeln!(f, "push %rbp")?;
                writeln!(f, "mov %rsp, %rbp")?;

                if max_register >= &Register::Num(NumRegister::R12) {
                    writeln!(f, "push {}", NumRegister64::R12)?;
                }

                if max_register >= &Register::Num(NumRegister::R13) {
                    writeln!(f, "push {}", NumRegister64::R13)?;
                }

                if max_register >= &Register::Num(NumRegister::R14) {
                    writeln!(f, "push {}", NumRegister64::R14)?;
                }

                if max_register >= &Register::Num(NumRegister::R15) {
                    writeln!(f, "push {}", NumRegister64::R15)?;
                }

                if let Register::Stack(StackRegister(registers)) = *max_register {
                    // Align to nearest 16 byte value
                    let bytes = (registers * 4 + 15) & !15;

                    writeln!(f, "sub {}, %rsp", Value::Immediate(bytes as i32))?;
                }

                for (idx, target) in params.iter().enumerate() {
                    let param_reg = FunctionArgRegister(idx);
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
                if max_register >= &Register::Num(NumRegister::R15) {
                    writeln!(f, "pop {}", NumRegister64::R15)?;
                }

                if max_register >= &Register::Num(NumRegister::R14) {
                    writeln!(f, "pop {}", NumRegister64::R14)?;
                }

                if max_register >= &Register::Num(NumRegister::R13) {
                    writeln!(f, "pop {}", NumRegister64::R13)?;
                }

                if max_register >= &Register::Num(NumRegister::R12) {
                    writeln!(f, "pop {}", NumRegister64::R12)?;
                }

                writeln!(f, "mov {}, {}", value, SystemRegister::Eax)?;
                writeln!(f, "leave")?;
                writeln!(f, "ret")
            }
            Instruction::CallFunction { dst, label, params } => {
                writeln!(f, "push {}", NumRegister64::Temp)?;
                writeln!(f, "push {}", NumRegister64::R9)?;
                writeln!(f, "push {}", NumRegister64::R10)?;
                writeln!(f, "push {}", NumRegister64::R11)?;
                
                let mut aligned_param_count = params.len();
                if aligned_param_count % 2 == 1 {
                    aligned_param_count += 1;
                    writeln!(f, "sub {}, %rsp", Value::Immediate(8))?;
                }
                for param in params.iter().rev() {
                    writeln!(f, "push {}", param)?;
                }
                writeln!(f, "call {}", label)?;
                if aligned_param_count != 0 {
                    writeln!(f, "add {}, %rsp", Value::Immediate(aligned_param_count as i32 * 8))?;
                }

                writeln!(f, "pop {}", NumRegister64::R11)?;
                writeln!(f, "pop {}", NumRegister64::R10)?;
                writeln!(f, "pop {}", NumRegister64::R9)?;
                writeln!(f, "pop {}", NumRegister64::Temp)?;
                if let Some(dst) = dst {
                    write!(f, "mov {}, {}", SystemRegister::Eax, dst)?
                }

                Ok(())
            }
            Instruction::GlobalLabel { label } => write!(f, ".global {}", label),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Register(Register),
    Immediate(i32),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Register(register) => register.fmt(f),
            Value::Immediate(value) => write!(f, "${}", value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Register {
    #[default]
    Temp,
    Num(NumRegister),
    Stack(StackRegister),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SystemRegister {
    Eax,
    Ebx,
    Ecx,
    Edx,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumRegister {
    // R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumRegister64 {
    Temp,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StackRegister(pub usize);

struct FunctionArgRegister(pub usize);

impl<T: Into<Register>> From<T> for Value {
    fn from(value: T) -> Self {
        Value::Register(value.into())
    }
}

impl From<NumRegister> for Register {
    fn from(value: NumRegister) -> Self {
        Self::Num(value)
    }
}

impl From<StackRegister> for Register {
    fn from(value: StackRegister) -> Self {
        Self::Stack(value)
    }
}

impl Display for SystemRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SystemRegister::Eax => write!(f, "%eax"),
            SystemRegister::Ebx => write!(f, "%ebx"),
            SystemRegister::Ecx => write!(f, "%ecx"),
            SystemRegister::Edx => write!(f, "%edx"),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Temp => write!(f, "%r8d"),
            Register::Num(num_register) => num_register.fmt(f),
            Register::Stack(stack_register) => stack_register.fmt(f),
        }
    }
}

impl Display for NumRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // NumRegister::R8 => write!(f, "%r8d"),
            NumRegister::R9 => write!(f, "%r9d"),
            NumRegister::R10 => write!(f, "%r10d"),
            NumRegister::R11 => write!(f, "%r11d"),
            NumRegister::R12 => write!(f, "%r12d"),
            NumRegister::R13 => write!(f, "%r13d"),
            NumRegister::R14 => write!(f, "%r14d"),
            NumRegister::R15 => write!(f, "%r15d"),
        }
    }
}

impl Display for NumRegister64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumRegister64::Temp => write!(f, "%r8"),
            NumRegister64::R9 => write!(f, "%r9"),
            NumRegister64::R10 => write!(f, "%r10"),
            NumRegister64::R11 => write!(f, "%r11"),
            NumRegister64::R12 => write!(f, "%r12"),
            NumRegister64::R13 => write!(f, "%r13"),
            NumRegister64::R14 => write!(f, "%r14"),
            NumRegister64::R15 => write!(f, "%r15"),
        }
    }
}

impl Display for StackRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "-{}(%rbp)", self.0 * 4)
    }
}

impl Display for FunctionArgRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let offset =  (self.0 + 1) * 8 + 8;
        write!(f, "{}(%rbp)", offset)
    }
}

impl GraphColor for Register {
    fn ascending_iter() -> impl Iterator<Item = Self> {
        [
            // Register::Num(NumRegister::R8),
            Register::Num(NumRegister::R9),
            Register::Num(NumRegister::R10),
            Register::Num(NumRegister::R11),
            Register::Num(NumRegister::R12),
            Register::Num(NumRegister::R13),
            Register::Num(NumRegister::R14),
            Register::Num(NumRegister::R15),
        ]
        .into_iter()
        .chain((1..).map(|i| Register::Stack(StackRegister(i))))
    }
}
