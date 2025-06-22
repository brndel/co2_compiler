use std::fmt::Display;

use crate::register_alloc::GraphColor;

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
pub struct StackRegister(pub usize);

pub struct FunctionArgRegister {
    idx: usize,
    get: bool,
}

impl FunctionArgRegister {
    pub fn set(idx: usize) -> Self {
        Self { idx, get: false }
    }

    pub fn get(idx: usize) -> Self {
        Self { idx, get: true }
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
        // 32 bytes for saved stack registers
        write!(f, "-{}(%rbp)", self.0 * 4 + 32)
    }
}

impl Display for FunctionArgRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.get {
            let offset = self.idx * 4 + 16;
            write!(f, "{}(%rbp)", offset)
        } else {
            let offset = self.idx * 4;
            write!(f, "{}(%rsp)", offset)    
        }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Register64 {
    #[default]
    Temp,
    Num(NumRegister64),
    Stack(StackRegister),
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumRegister64 {
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Display for Register64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register64::Temp => write!(f, "%r8"),
            Register64::Num(num_register) => num_register.fmt(f),
            Register64::Stack(stack_register) => stack_register.fmt(f),
        }
    }
}

impl Display for NumRegister64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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


impl From<Register> for Register64 {
    fn from(value: Register) -> Self {
        match value {
            Register::Temp => Register64::Temp,
            Register::Num(num_register) => Register64::Num(num_register.into()),
            Register::Stack(stack_register) => Register64::Stack(stack_register),
        }
    }
}

impl From<NumRegister> for NumRegister64 {
    fn from(value: NumRegister) -> Self {
        match value {
            NumRegister::R9 => NumRegister64::R9,
            NumRegister::R10 => NumRegister64::R10,
            NumRegister::R11 => NumRegister64::R11,
            NumRegister::R12 => NumRegister64::R12,
            NumRegister::R13 => NumRegister64::R13,
            NumRegister::R14 => NumRegister64::R14,
            NumRegister::R15 => NumRegister64::R15,
        }
    }
}