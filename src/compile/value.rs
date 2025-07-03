use std::fmt::Display;

use crate::compile::{Register, register::Register64};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Register(Register),
    Immediate(i32),
}

impl Value {
    pub fn is_stack(&self) -> bool {
        match self {
            Value::Register(register) => register.is_stack(),
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Register(register) => register.fmt(f),
            Value::Immediate(value) => write!(f, "${}", value),
        }
    }
}

impl<T: Into<Register>> From<T> for Value {
    fn from(value: T) -> Self {
        Value::Register(value.into())
    }
}

impl Value {
    #[allow(dead_code)]
    pub fn to_64(self) -> Value64 {
        match self {
            Value::Register(register) => Value64::Register(register.into()),
            Value::Immediate(imm) => Value64::Immediate(imm),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Value64 {
    Register(Register64),
    Immediate(i32),
}

impl Display for Value64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value64::Register(register) => register.fmt(f),
            Value64::Immediate(value) => write!(f, "${}", value),
        }
    }
}

impl PartialEq<Register> for Value {
    fn eq(&self, other: &Register) -> bool {
        match self {
            Value::Register(register) => register == other,
            Value::Immediate(_) => false,
        }
    }
}

impl PartialEq<Register64> for Value64 {
    fn eq(&self, other: &Register64) -> bool {
        match self {
            Value64::Register(register) => register == other,
            Value64::Immediate(_) => false,
        }
    }
}
