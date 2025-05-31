use std::fmt::Display;

use crate::{parser::Block, register_alloc::GraphColor, ssa::BlockLabel};

pub enum Instruction {
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
    AllocateStack {
        bytes: u32,
    },
    Return {
        value: Value,
    },
    Label {
        label: BlockLabel,
    },
    Jump {
        dst: BlockLabel,
    },
    JumpConditional {
        condition: Register,
        on_true: BlockLabel,
        on_false: BlockLabel,
    },
}

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

impl Display for Instruction {
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
                    Value::Register(register) => writeln!(f, "idiv {}", register)?,
                    value @ Value::Immediate(_) => {
                        writeln!(f, "mov {}, {}", value, reg)?;
                        writeln!(f, "idiv {}", reg)?;
                    }
                }
                write!(f, "movl {}, {}", SystemRegister::Eax, reg)
            }
            Instruction::Mod { reg, value } => {
                writeln!(f, "movl {}, {}", reg, SystemRegister::Eax)?;
                writeln!(f, "cltd")?;
                match value {
                    Value::Register(register) => writeln!(f, "idiv {}", register)?,
                    value @ Value::Immediate(_) => {
                        writeln!(f, "mov {}, {}", value, reg)?;
                        writeln!(f, "idiv {}", reg)?;
                    }
                }
                write!(f, "movl {}, {}", SystemRegister::Edx, reg)
            }
            Instruction::BitAnd { reg, value } => {
                write!(f, "andl {}, {}", reg, value)
            }
            Instruction::BitOr { reg, value } => {
                write!(f, "orl {}, {}", reg, value)
            }
            Instruction::BitXor { reg, value } => {
                write!(f, "xorl {}, {}", reg, value)
            }
            Instruction::ShiftLeft { reg, value } => {
                writeln!(f, "movl {}, {}", SystemRegister::Ecx, value)?;
                write!(f, "sall %cl, {}", reg)
            }
            Instruction::ShiftRight { reg, value } => {
                writeln!(f, "movl {}, {}", SystemRegister::Ecx, value)?;
                write!(f, "sarl %cl, {}", reg)
            }
            Instruction::Compare { op, target, a, b } => {
                writeln!(f, "cmp {}, {}", b, a)?;
                writeln!(f, "{} %al", op)?;
                write!(f, "movzbl %al, {}", target)
            }
            Instruction::Negate { reg } => write!(f, "neg {}", reg),
            Instruction::BitNot { reg } => write!(f, "notl {}", reg),
            Instruction::Return { value } => {
                writeln!(f, "mov {}, {}", value, SystemRegister::Eax)?;
                writeln!(f, "leave")?;
                write!(f, "ret")
            }
            Instruction::AllocateStack { bytes } => {
                writeln!(f, "push %rbp")?;
                writeln!(f, "mov %rsp, %rbp")?;
                write!(f, "sub {}, %rsp", Value::Immediate(*bytes as i32))
            }
            Instruction::Label { label } => {
                writeln!(f, "")?;
                if label.id() == 0 {
                    write!(f, "# ")?;
                }
                
                write!(f, "{}:", label)
            },
            Instruction::Jump { dst } => {
                write!(f, "jmp {}", dst)
            },
            Instruction::JumpConditional {
                condition,
                on_true,
                on_false,
            } => {
                writeln!(f, "test {}, {}", condition, condition)?;
                writeln!(f, "jz {}", on_false)?;
                write!(f, "jmp {}", on_true)
            },
        }
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
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
pub struct StackRegister(pub u32);

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

impl Display for StackRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "-{}(%rbp)", self.0 * 4)
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
