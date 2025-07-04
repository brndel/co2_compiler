use std::fmt::Display;

use crate::{compile::byte_size::WithByteSizeExt, register_alloc::GraphColor};

use super::{
    byte_size::WithByteSize, ByteSize
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    Num(NumRegister),
    Stack(StackRegister),
    System(SystemRegister),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(dead_code)]
pub enum SystemRegister {
    Eax,
    Ebx,
    Ecx,
    Edx,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumRegister {
    Temp,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Register {
    pub fn is_stack(&self) -> bool {
        match self {
            Register::Stack(_) => true,
            _ => false,
        }
    }
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
            Register::Num(num_register) => num_register.fmt(f),
            Register::Stack(stack_register) => stack_register.fmt(f),
            Register::System(system_register) => system_register.fmt(f),
        }
    }
}

impl Display for NumRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.with_size(ByteSize::B4))
    }
}

impl Display for StackRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // 32 bytes for saved stack registers
        write!(f, "-{}(%rbp)", self.0 * 8 + 32)
    }
}

impl Display for FunctionArgRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.get {
            let offset = self.idx * 8 + 16;
            write!(f, "{}(%rbp)", offset)
        } else {
            let offset = self.idx * 8;
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

impl Display for WithByteSize<Register> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Register::Num(num_register) => write!(f, "{}", num_register.with_size(self.size)),
            Register::Stack(stack_register) => write!(f, "{}", stack_register),
            Register::System(system_register) => write!(f, "{}", system_register.with_size(self.size)),
        }
    }
}


impl Display for WithByteSize<SystemRegister> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg = {
            macro_rules! format_sys_register {
            ($register:expr, $size:expr, { $($reg:pat => $base:expr),* }) => {
                match ($register, $size) {
                    $(
                        ($reg, ByteSize::B1) => concat!("%", $base, "l"),
                        ($reg, ByteSize::B2) => concat!("%", $base, "x"),
                        ($reg, ByteSize::B4) => concat!("%e", $base, "x"),
                        ($reg, ByteSize::B8) => concat!("%r", $base, "x"),
                    )*
                }
            };
        }

            format_sys_register!(self.value, self.size, {
                SystemRegister::Eax => "a",
                SystemRegister::Ebx => "b",
                SystemRegister::Ecx => "c",
                SystemRegister::Edx => "d"
            })
        };

        write!(f, "{}", reg)
    }
}

impl Display for WithByteSize<NumRegister> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg = {
            macro_rules! format_num_register {
            ($register:expr, $size:expr, { $($reg:pat => $base:expr),* }) => {
                match ($register, $size) {
                    $(
                        ($reg, ByteSize::B1) => concat!("%r", $base, "b"),
                        ($reg, ByteSize::B2) => concat!("%r", $base, "w"),
                        ($reg, ByteSize::B4) => concat!("%r", $base, "d"),
                        ($reg, ByteSize::B8) => concat!("%r", $base),
                    )*
                }
            };
        }

            format_num_register!(self.value, self.size, {
                NumRegister::Temp => "8",
                NumRegister::R9 => "9",
                NumRegister::R10 => "10",
                NumRegister::R11 => "11",
                NumRegister::R12 => "12",
                NumRegister::R13 => "13",
                NumRegister::R14 => "14",
                NumRegister::R15 => "15"
            })
        };

        write!(f, "{}", reg)
    }
}
