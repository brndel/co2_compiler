use std::collections::{BTreeMap, BTreeSet};

use crate::{
    compile::{
        instruction::{BuiltinFuntion, FunctionPointer}, value::{Value}
    },
    lexer::{BinaryOperator, UnaryOperator},
    parser::FunctionIdent,
    semantic::StructNamespace,
    ssa::{BasicBlockEnd, BlockLabel, FunctionIrGraph, SsaInstruction, SsaValue, VirtualRegister},
};

use super::{
    instruction::{CompareOp, Instruction}, register::NumRegister, Register
};

pub fn generate_asm<'a>(
    func: &FunctionIrGraph<'a>,
    registers: &BTreeMap<VirtualRegister, Register>,
    visited_blocks: &BTreeSet<BlockLabel<'a>>,
    func_labels: &BTreeMap<&'a str, BlockLabel<'a>>,
    structs: &StructNamespace<'a>,
) -> Vec<Instruction<'a>> {
    let mut instructions = Vec::new();

    let max_register = registers.values().max().cloned().unwrap_or(Register::Num(NumRegister::Temp));

    for block in &func.graph {
        if !visited_blocks.contains(&block.label) {
            continue;
        }

        if block.label.id() == 0 {
            instructions.push(Instruction::GlobalLabel { label: block.label });
            instructions.push(Instruction::Label { label: block.label });
            let params = func.params.iter().map(|param| registers[param]).collect();
            instructions.push(Instruction::FunctionProlog {
                max_register,
                params,
            });
        } else {
            instructions.push(Instruction::Label { label: block.label });
        }

        for instr in &block.instructions {
            match instr {
                SsaInstruction::Move { target, source } => {
                    let dst = registers[&target];
                    let src = transform_value(source, registers);

                    instructions.push(Instruction::Move { src, dst });
                }
                SsaInstruction::PhiMove { target, source } => {
                    let dst = registers[&target];
                    let src = transform_value(source, registers);

                    instructions.push(Instruction::Move { src, dst });
                }
                SsaInstruction::BinaryOp { target, a, op, b } => {
                    let dst = registers[&target];
                    let a = transform_value(a, registers);
                    let b = transform_value(b, registers);

                    instructions.push(Instruction::Move {
                        src: a,
                        dst: Register::Num(NumRegister::Temp),
                    });
                    let mut move_dst = true;
                    match op {
                        BinaryOperator::Plus => instructions.push(Instruction::Add {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::Minus => instructions.push(Instruction::Sub {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::Mul => instructions.push(Instruction::Mul {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::Div => instructions.push(Instruction::Div {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::Mod => instructions.push(Instruction::Mod {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::LogicAnd => instructions.push(Instruction::BitAnd {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::LogicOr => instructions.push(Instruction::BitOr {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::BitAnd => instructions.push(Instruction::BitAnd {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::BitOr => instructions.push(Instruction::BitOr {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::BitXor => instructions.push(Instruction::BitXor {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::ShiftLeft => instructions.push(Instruction::ShiftLeft {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::ShiftRight => instructions.push(Instruction::ShiftRight {
                            reg: Register::Num(NumRegister::Temp),
                            value: b,
                        }),
                        BinaryOperator::Less => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::Less,
                                target: dst,
                                a: Register::Num(NumRegister::Temp),
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::LessEq => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::LessEq,
                                target: dst,
                                a: Register::Num(NumRegister::Temp),
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::Greater => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::Greater,
                                target: dst,
                                a: Register::Num(NumRegister::Temp),
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::GreaterEq => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::GreaterEq,
                                target: dst,
                                a: Register::Num(NumRegister::Temp),
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::Equals => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::Equals,
                                target: dst,
                                a: Register::Num(NumRegister::Temp),
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::NotEquals => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::NotEquals,
                                target: dst,
                                a: Register::Num(NumRegister::Temp),
                                b,
                            });
                            move_dst = false;
                        }
                    }
                    if move_dst {
                        instructions.push(Instruction::Move {
                            src: NumRegister::Temp.into(),
                            dst,
                        });
                    }
                }
                SsaInstruction::UnaryOp { target, op, value } => {
                    let reg = registers[&target];
                    let value = registers[&value];

                    instructions.push(Instruction::Move {
                        src: value.into(),
                        dst: reg,
                    });
                    match op {
                        UnaryOperator::Minus => {
                            instructions.push(Instruction::Negate { reg });
                        }
                        UnaryOperator::LogicNot => instructions.push(Instruction::LogicNot { reg }),
                        UnaryOperator::BitNot => instructions.push(Instruction::BitNot { reg }),
                    }
                }
                SsaInstruction::FunctionCall { target, name, args } => {
                    let dst = target.map(|target| registers[&target]);
                    let params = args
                        .into_iter()
                        .map(|arg| transform_value(arg, registers))
                        .collect();
                    let func = match name {
                        FunctionIdent::Print => FunctionPointer::Builtin(BuiltinFuntion::Print),
                        FunctionIdent::Read => FunctionPointer::Builtin(BuiltinFuntion::Read),
                        FunctionIdent::Flush => FunctionPointer::Builtin(BuiltinFuntion::Flush),
                        FunctionIdent::User(name) => FunctionPointer::User {
                            label: func_labels[name],
                        },
                    };
                    instructions.push(Instruction::CallFunction { dst, func, params });
                }
                SsaInstruction::Allocate {
                    target,
                    ty,
                    array_len,
                } => {
                    let size = structs.get_size(ty);

                    let dst = target.map(|target| registers[&target]);
                    let array_len = array_len.map(|value| transform_value(&value, registers));

                    instructions.push(Instruction::Alloc {
                        byte_count: size.byte_count,
                        dst,
                        array_len,
                    });
                }
                SsaInstruction::MemGet {
                    target,
                    source_ptr,
                    offset,
                    field_size
                } => {
                    let target = registers[target];
                    let source_ptr = transform_value(source_ptr, registers);
                    instructions.push(Instruction::MemGet {
                        target,
                        source_ptr,
                        offset: *offset,
                        field_size: *field_size
                    });
                }
                SsaInstruction::MemSet {
                    target_ptr,
                    source,
                    offset,
                    field_size
                } => {
                    let target_ptr = registers[target_ptr];
                    let source = transform_value(source, registers);
                    instructions.push(Instruction::MemSet {
                        target_ptr,
                        source,
                        offset: *offset,
                        field_size: *field_size
                    });
                }
                SsaInstruction::CalcArrayPtr {
                    target,
                    ptr,
                    index,
                    struct_size,
                } => {
                    let array_ptr = transform_value(ptr, registers);
                    let index = transform_value(index, registers);
                    instructions.push(Instruction::CheckArrayLen { array_ptr, index });

                    instructions.push(Instruction::Move { src: index, dst: Register::Num(NumRegister::Temp) });
                    instructions.push(Instruction::Mul { reg: Register::Num(NumRegister::Temp), value: Value::Immediate(*struct_size as i32) });
                    instructions.push(Instruction::Add64 { reg: Register::Num(NumRegister::Temp), value: array_ptr });

                    let target = registers[target];
                    instructions.push(Instruction::Move { src: Value::Register(Register::Num(NumRegister::Temp)), dst: target.into() });

                },
                SsaInstruction::FunctionArg { .. } => (),
            }
        }

        match &block.end {
            BasicBlockEnd::Goto { label } => {
                instructions.push(Instruction::Jump { dst: *label });
            }
            BasicBlockEnd::Return { value } => {
                instructions.push(Instruction::Return {
                    value: transform_value(value, registers),
                    max_register,
                });
            }
            BasicBlockEnd::ConditionalJump {
                condition,
                on_true,
                on_false,
            } => {
                let condition = match transform_value(condition, registers) {
                    Value::Register(register) => register,
                    value @ Value::Immediate(_) => {
                        instructions.push(Instruction::Move {
                            src: value,
                            dst: Register::Num(NumRegister::Temp),
                        });
                        Register::Num(NumRegister::Temp)
                    }
                };
                instructions.push(Instruction::JumpConditional {
                    condition,
                    on_true: *on_true,
                    on_false: *on_false,
                });
            }
        };
    }

    instructions
}

fn transform_value(value: &SsaValue, registers: &BTreeMap<VirtualRegister, Register>) -> Value {
    match value {
        SsaValue::Register(virtual_register) => Value::Register(registers[&virtual_register]),
        SsaValue::ImmediateNum(value) => Value::Immediate(*value),
        SsaValue::ImmediateBool(value) => Value::Immediate(value.then_some(1).unwrap_or(0)),
    }
}
