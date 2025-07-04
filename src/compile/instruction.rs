use std::fmt::{Display, Formatter};

use crate::{
    compile::{
        ByteSize, Register,
        byte_size::WithByteSizeExt,
        register::{FunctionArgRegister, StackRegister, SystemRegister},
        value::Value,
    },
    ssa::{BlockLabel, SsaValue},
    util::align_bytes,
};

use super::{byte_size::WithByteSize, register::NumRegister};

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
    Add64 {
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
    Alloc {
        byte_count: usize,
        dst: Option<Register>,
        array_len: Option<Value>,
    },
    MemGet {
        target: Register,
        source_ptr: Value,
        offset: usize,
        field_size: ByteSize,
    },
    MemSet {
        target_ptr: Register,
        source: Value,
        offset: usize,
        field_size: ByteSize,
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
                    return Ok(());
                }
                let size = if src.is_immediate() {
                    ByteSize::B4
                } else {
                    ByteSize::B8
                };

                if src.is_stack() && dst.is_stack() {
                    writeln!(
                        f,
                        "{} {}, {}",
                        MoveInstr.with_size(size),
                        src.with_size(size),
                        NumRegister::Temp.with_size(size)
                    )?;
                    write!(
                        f,
                        "{} {}, {}",
                        MoveInstr.with_size(size),
                        NumRegister::Temp.with_size(size),
                        dst.with_size(size)
                    )
                } else {
                    write!(
                        f,
                        "{} {}, {}",
                        MoveInstr.with_size(size),
                        src.with_size(size),
                        dst.with_size(size)
                    )
                }
            }
            Instruction::Add { reg, value } => write!(f, "addl {}, {}", value, reg),
            Instruction::Add64 { reg, value } => write!(
                f,
                "add {}, {}",
                value.with_size(ByteSize::B8),
                reg.with_size(ByteSize::B8)
            ),
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
                    writeln!(f, "test {}, {}", Value::Immediate(1), condition)?;
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

                writeln!(f, "push {}", NumRegister::R12.with_size(ByteSize::B8))?;
                writeln!(f, "push {}", NumRegister::R13.with_size(ByteSize::B8))?;
                writeln!(f, "push {}", NumRegister::R14.with_size(ByteSize::B8))?;
                writeln!(f, "push {}", NumRegister::R15.with_size(ByteSize::B8))?;

                if let Register::Stack(StackRegister(registers)) = *max_register {
                    let bytes = align_bytes(registers * 8, 16);

                    writeln!(f, "sub {}, %rsp", Value::Immediate(bytes as i32))?;
                }

                for (idx, target) in params.iter().enumerate() {
                    let param_reg = FunctionArgRegister::get(idx);
                    if let &Register::Stack(_) = target {
                        writeln!(
                            f,
                            "mov {}, {}",
                            param_reg,
                            NumRegister::Temp.with_size(ByteSize::B8)
                        )?;
                        writeln!(
                            f,
                            "mov {}, {}",
                            NumRegister::Temp.with_size(ByteSize::B8),
                            target.with_size(ByteSize::B8)
                        )?;
                    } else {
                        writeln!(f, "mov {}, {}", param_reg, target.with_size(ByteSize::B8))?;
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
                    let bytes = (registers * 8 + 15) & !15;

                    writeln!(f, "add {}, %rsp", Value::Immediate(bytes as i32))?;
                }

                writeln!(
                    f,
                    "mov {}, {}",
                    value.with_size(ByteSize::B8),
                    SystemRegister::Eax.with_size(ByteSize::B8)
                )?;

                writeln!(f, "pop {}", NumRegister::R15.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::R14.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::R13.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::R12.with_size(ByteSize::B8))?;

                writeln!(f, "leave")?;
                writeln!(f, "ret")
            }
            Instruction::CallFunction { dst, func, params } => {
                writeln!(
                    f,
                    "push {}",
                    Register::Num(NumRegister::Temp).with_size(ByteSize::B8)
                )?;
                writeln!(f, "push {}", NumRegister::R9.with_size(ByteSize::B8))?;
                writeln!(f, "push {}", NumRegister::R10.with_size(ByteSize::B8))?;
                writeln!(f, "push {}", NumRegister::R11.with_size(ByteSize::B8))?;

                match func {
                    FunctionPointer::User { label } => {
                        let param_bytes = (params.len() * 8 + 15) & !15;
                        writeln!(f, "sub {}, %rsp", Value::Immediate(param_bytes as i32))?;

                        for (idx, param) in params.iter().enumerate() {
                            let target = FunctionArgRegister::set(idx);
                            let size = if param.is_immediate() {
                                ByteSize::B4
                            } else {
                                ByteSize::B8
                            };
                            if let &Value::Register(Register::Stack(_)) = param {
                                writeln!(
                                    f,
                                    "{} {}, {}",
                                    MoveInstr.with_size(size),
                                    param,
                                    NumRegister::Temp.with_size(size)
                                )?;
                                writeln!(
                                    f,
                                    "{} {}, {}",
                                    MoveInstr.with_size(size),
                                    NumRegister::Temp.with_size(size),
                                    target
                                )?;
                            } else {
                                writeln!(
                                    f,
                                    "{} {}, {}",
                                    MoveInstr.with_size(size),
                                    param.with_size(size),
                                    target
                                )?;
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

                writeln!(f, "pop {}", NumRegister::R11.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::R10.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::R9.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::Temp.with_size(ByteSize::B8))?;
                if let Some(dst) = dst {
                    writeln!(
                        f,
                        "mov {}, {}",
                        SystemRegister::Eax.with_size(ByteSize::B8),
                        dst.with_size(ByteSize::B8)
                    )?
                }

                Ok(())
            }
            Instruction::Alloc {
                byte_count,
                dst,
                array_len,
            } => {
                if let Some(len) = array_len {
                    writeln!(f, "movl {}, %edi", len)?;
                    writeln!(f, "test %edi, %edi")?;
                    writeln!(f, "js call_abort")?;
                }

                writeln!(f, "push {}", NumRegister::Temp.with_size(ByteSize::B8))?;
                writeln!(f, "push {}", NumRegister::R9.with_size(ByteSize::B8))?;
                writeln!(f, "push {}", NumRegister::R10.with_size(ByteSize::B8))?;
                writeln!(f, "push {}", NumRegister::R11.with_size(ByteSize::B8))?;

                if let Some(_) = array_len {
                    let byte_count = byte_count + 8;
                    writeln!(f, "mov {}, %rsi", Value::Immediate(byte_count as i32))?;
                    writeln!(f, "call calloc")?;

                    writeln!(
                        f,
                        "add {}, {}",
                        Value::Immediate(8),
                        SystemRegister::Eax.with_size(ByteSize::B8)
                    )?;
                } else {
                    writeln!(f, "movl {}, %edi", Value::Immediate(1))?;
                    writeln!(f, "mov {}, %rsi", Value::Immediate(*byte_count as i32))?;
                    writeln!(f, "call calloc")?;
                }

                writeln!(f, "pop {}", NumRegister::R11.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::R10.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::R9.with_size(ByteSize::B8))?;
                writeln!(f, "pop {}", NumRegister::Temp.with_size(ByteSize::B8))?;

                if let Some(len) = array_len {
                    writeln!(
                        f,
                        "movl {}, -8({})",
                        len,
                        SystemRegister::Eax.with_size(ByteSize::B8)
                    )?;
                }

                if let Some(dst) = dst {
                    writeln!(
                        f,
                        "mov {}, {}",
                        SystemRegister::Eax.with_size(ByteSize::B8),
                        dst.with_size(ByteSize::B8)
                    )?
                }

                Ok(())
            }
            Instruction::MemGet {
                target,
                source_ptr,
                offset,
                field_size,
            } => {
                let source_ptr =
                    ensure_ptr_not_on_stack(*source_ptr, Register::System(SystemRegister::Eax), f)?;

                let is_stack = target.is_stack();

                let real_target = target;
                let target = if is_stack {
                    Register::System(SystemRegister::Ebx)
                } else {
                    *target
                };

                writeln!(
                    f,
                    "{} {}({}), {}",
                    MoveInstr.with_size(*field_size),
                    offset,
                    source_ptr.with_size(ByteSize::B8),
                    target.with_size(*field_size)
                )?;

                if is_stack {
                    write!(
                        f,
                        "{} {}, {}",
                        MoveInstr.with_size(*field_size),
                        target.with_size(*field_size),
                        real_target
                    )?;
                }

                Ok(())
            }
            Instruction::MemSet {
                target_ptr,
                source,
                offset,
                field_size,
            } => {
                let source = if source.is_stack() {
                    writeln!(
                        f,
                        "{} {}, {}",
                        MoveInstr.with_size(*field_size),
                        source.with_size(*field_size),
                        SystemRegister::Ebx.with_size(*field_size)
                    )?;
                    Value::Register(Register::System(SystemRegister::Ebx))
                } else {
                    *source
                };

                let target_ptr = ensure_ptr_not_on_stack(
                    Value::Register(*target_ptr),
                    Register::System(SystemRegister::Eax),
                    f,
                )?;

                write!(
                    f,
                    "{} {}, {}({})",
                    MoveInstr.with_size(*field_size),
                    source.with_size(*field_size),
                    offset,
                    target_ptr.with_size(ByteSize::B8)
                )
            }
            Instruction::CheckArrayLen { array_ptr, index } => {
                writeln!(f, "# check array index {} of array at {}", index, array_ptr)?;

                // idx < 0
                writeln!(
                    f,
                    "movl {}, {}",
                    index.with_size(ByteSize::B4),
                    SystemRegister::Eax.with_size(ByteSize::B4)
                )?;

                writeln!(f, "test {}, {}", SystemRegister::Eax, SystemRegister::Eax)?;
                writeln!(f, "js call_abort")?;

                // idx >= len
                let array_ptr =
                    ensure_ptr_not_on_stack(*array_ptr, Register::System(SystemRegister::Eax), f)?;
                writeln!(
                    f,
                    "movl -8({}), {}",
                    array_ptr.with_size(ByteSize::B8),
                    SystemRegister::Eax
                )?;

                writeln!(f, "cmp {}, {}", index, SystemRegister::Eax)?;
                writeln!(f, "jle call_abort")?;

                Ok(())
            }
            Instruction::GlobalLabel { label } => write!(f, ".global {}", label),
        }
    }
}

fn ensure_ptr_not_on_stack(
    ptr: Value,
    register: Register,
    f: &mut Formatter<'_>,
) -> Result<Value, std::fmt::Error> {
    if ptr.is_stack() {
        writeln!(
            f,
            "movq {}, {}",
            ptr.with_size(ByteSize::B8),
            register.with_size(ByteSize::B8)
        )?;
        Ok(Value::Register(register))
    } else {
        Ok(ptr)
    }
}

#[derive(Clone, Copy)]
struct MoveInstr;

impl Display for WithByteSize<MoveInstr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.size {
            ByteSize::B1 => write!(f, "movb"),
            ByteSize::B2 => write!(f, "movw"),
            ByteSize::B4 => write!(f, "movl"),
            ByteSize::B8 => write!(f, "movq"),
        }
    }
}
#[derive(Clone, Copy)]
struct MoveInstrExtendZero;

impl Display for WithByteSize<MoveInstrExtendZero> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.size {
            ByteSize::B1 => write!(f, "movzbl"),
            ByteSize::B2 => write!(f, "movzwl"),
            _ => unimplemented!(),
        }
    }
}
