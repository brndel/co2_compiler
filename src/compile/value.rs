use std::fmt::Display;

use crate::compile::{register::Register64, Register};


#[derive(Debug, Clone, Copy)]
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

impl<T: Into<Register>> From<T> for Value {
    fn from(value: T) -> Self {
        Value::Register(value.into())
    }
}

impl Value {
    pub fn to_64(self) -> Value64 {
        match self {
            Value::Register(register) => Value64::Register(register.into()),
            Value::Immediate(imm) => Value64::Immediate(imm),
        }
    }
}

#[derive(Debug, Clone, Copy)]
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