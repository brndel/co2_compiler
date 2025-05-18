use std::fmt::Display;

pub enum Instruction {
    Move { src: Value, dst: Register },
    Add { reg: Register, value: Value },
    Sub { reg: Register, value: Value },
    Mul { reg: Register, value: Value },
    Negate { reg: Register },
    Div { register: NumRegister, value: NumRegister },
    Mod { register: NumRegister, value: NumRegister },
    Return { register: Register },
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Move { src, dst } => write!(f, "movl {}, {}", src, dst),
            Instruction::Add { reg, value } => write!(f, "add {}, {}", value, reg),
            Instruction::Sub { reg, value } => write!(f, "sub {}, {}", value, reg),
            Instruction::Mul { reg, value } => write!(f, "imul {}, {}", value, reg),
            Instruction::Negate { reg } => write!(f, "neg {}", reg),
            Instruction::Div { register, value } => {
                writeln!(f, "movl {}, {}", register, Register::Eax)?;
                writeln!(f, "cltd")?;
                writeln!(f, "idiv {}", value)?;
                write!(f, "movl {}, {}", Register::Eax, register)
            }
            Instruction::Mod { register, value } => {
                writeln!(f, "movl {}, {}", register, Register::Eax)?;
                writeln!(f, "cltd")?;
                writeln!(f, "idiv {}", value)?;
                write!(f, "movl {}, {}", Register::Edx, register)
            }
            Instruction::Return {
                register
            } => {
                if register != &Register::Eax {
                    writeln!(f, "mov {}, {}", Register::Eax, register)?;
                }
                write!(f, "ret")
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Eax,
    Ebx,
    Ecx,
    Edx,
    Num(NumRegister),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumRegister {
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl From<NumRegister> for Register {
    fn from(value: NumRegister) -> Self {
        Self::Num(value)
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Eax => write!(f, "%eax"),
            Register::Ebx => write!(f, "%ebx"),
            Register::Ecx => write!(f, "%ecx"),
            Register::Edx => write!(f, "%edx"),
            Register::Num(num_register) => num_register.fmt(f),
        }
    }
}

impl Display for NumRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumRegister::R8 => write!(f, "%r8d"),
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
