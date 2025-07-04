use std::fmt::Display;

use crate::compile::{Register};

use super::byte_size::{WithByteSize, WithByteSizeExt};

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

    pub fn is_immediate(&self) -> bool {
        match self {
            Value::Immediate(_) => true,
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

impl Display for WithByteSize<Value> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Register(register) => write!(f, "{}", register.with_size(self.size)),
            Value::Immediate(value) => write!(f, "${}", value),
        }
    }
}