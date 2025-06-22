use std::{cell::Ref, fmt::Display};

use crate::{
    compile::{
        Register,
        register::{
            FunctionArgRegister, NumRegister, NumRegister64, Register64, StackRegister,
            SystemRegister,
        },
        value::Value,
    },
    register_alloc::GraphColor,
    ssa::{BlockLabel, VirtualRegister},
};

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
        func: FunctionPointer<'a>,
        dst: Option<Register>,
        params: Vec<Value>,
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
            Instruction::CallFunction { dst, func, params } => {
                writeln!(f, "push {}", Register64::Temp)?;
                writeln!(f, "push {}", NumRegister64::R9)?;
                writeln!(f, "push {}", NumRegister64::R10)?;
                writeln!(f, "push {}", NumRegister64::R11)?;

                match func {
                    FunctionPointer::User { label } => {
                        let mut aligned_param_count = params.len();
                        if aligned_param_count % 2 == 1 {
                            aligned_param_count += 1;
                            writeln!(f, "sub {}, %rsp", Value::Immediate(8))?;
                        }
                        for param in params.iter().rev() {
                            writeln!(f, "push {}", (*param).to_64())?;
                        }
                        writeln!(f, "call {}", label)?;
                        if aligned_param_count != 0 {
                            writeln!(
                                f,
                                "add {}, %rsp",
                                Value::Immediate(aligned_param_count as i32 * 8)
                            )?;
                        }
                    }
                    FunctionPointer::Builtin(builtin_funtion) => {
                        match builtin_funtion {
                            BuiltinFuntion::Print => {
                                let param = params[0];
                                writeln!(f, "mov {}, {}", param, SystemRegister::Eax)?;
                                writeln!(f, "call putchar")?;
                            },
                            BuiltinFuntion::Read => {
                                writeln!(f, "call getchar")?;
                            },
                            BuiltinFuntion::Flush => {
                                writeln!(f, "mov stdout(%rip), %rdi")?;
                                writeln!(f, "call fflush")?;
                                writeln!(f, "add {}, %rsp", Value::Immediate(8))?;
                            },
                        }
                    },
                }

                writeln!(f, "pop {}", NumRegister64::R11)?;
                writeln!(f, "pop {}", NumRegister64::R10)?;
                writeln!(f, "pop {}", NumRegister64::R9)?;
                writeln!(f, "pop {}", Register64::Temp)?;
                if let Some(dst) = dst {
                    write!(f, "mov {}, {}", SystemRegister::Eax, dst)?
                }

                Ok(())
            }
            Instruction::GlobalLabel { label } => write!(f, ".global {}", label),
        }
    }
}
