use std::collections::{BTreeMap, BTreeSet};

use crate::{
    compile::{instruction::{BuiltinFuntion, FunctionPointer}, value::Value}, lexer::{BinaryOperator, Operator, UnaryOperator}, ssa::{
        BasicBlock, BasicBlockEnd, BlockLabel, FunctionIrGraph, IrGraph, SsaInstruction, SsaValue, VirtualRegister
    }
};

use super::{
    Register,
    instruction::{CompareOp, Instruction},
};

pub fn generate_asm<'a>(
    func: &FunctionIrGraph<'a>,
    registers: &BTreeMap<VirtualRegister, Register>,
    visited_blocks: &BTreeSet<BlockLabel<'a>>,
    func_labels: &BTreeMap<&'a str, BlockLabel<'a>>,
) -> Vec<Instruction<'a>> {
    let mut instructions = Vec::new();

    let max_register = registers.values().max().cloned().unwrap_or_default();


    for block in &func.graph {
        if !visited_blocks.contains(&block.label) {
            continue;
        }

        if block.label.id() == 0 {
            instructions.push(Instruction::GlobalLabel { label: block.label });
            instructions.push(Instruction::Label { label: block.label });
            let params = func.params.iter().map(|param| registers[param]).collect();
            instructions.push(Instruction::FunctionProlog { max_register, params });
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
                        dst: Register::Temp,
                    });
                    let mut move_dst = true;
                    match op {
                        BinaryOperator::Plus => instructions.push(Instruction::Add {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::Minus => instructions.push(Instruction::Sub {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::Mul => instructions.push(Instruction::Mul {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::Div => instructions.push(Instruction::Div {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::Mod => instructions.push(Instruction::Mod {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::LogicAnd => instructions.push(Instruction::BitAnd {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::LogicOr => instructions.push(Instruction::BitOr {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::BitAnd => instructions.push(Instruction::BitAnd {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::BitOr => instructions.push(Instruction::BitOr {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::BitXor => instructions.push(Instruction::BitXor {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::ShiftLeft => instructions.push(Instruction::ShiftLeft {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::ShiftRight => instructions.push(Instruction::ShiftRight {
                            reg: Register::Temp,
                            value: b,
                        }),
                        BinaryOperator::Less => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::Less,
                                target: dst,
                                a: Register::Temp,
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::LessEq => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::LessEq,
                                target: dst,
                                a: Register::Temp,
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::Greater => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::Greater,
                                target: dst,
                                a: Register::Temp,
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::GreaterEq => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::GreaterEq,
                                target: dst,
                                a: Register::Temp,
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::Equals => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::Equals,
                                target: dst,
                                a: Register::Temp,
                                b,
                            });
                            move_dst = false;
                        }
                        BinaryOperator::NotEquals => {
                            instructions.push(Instruction::Compare {
                                op: CompareOp::NotEquals,
                                target: dst,
                                a: Register::Temp,
                                b,
                            });
                            move_dst = false;
                        }
                    }
                    if move_dst {
                        instructions.push(Instruction::Move {
                            src: Register::Temp.into(),
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
                SsaInstruction::FunctionArg { index, target } => {
                    
                }
                SsaInstruction::FunctionCall { target, name, args } => {
                    let dst = target.map(|target| registers[&target]);
                    let params = args.into_iter().map(|arg| transform_value(arg, registers)).collect();
                    let func = match *name {
                        "print" => FunctionPointer::Builtin(BuiltinFuntion::Print),
                        "read" => FunctionPointer::Builtin(BuiltinFuntion::Read),
                        "flush" => FunctionPointer::Builtin(BuiltinFuntion::Flush),
                        _ => FunctionPointer::User { label: func_labels[name] }
                    };
                    instructions.push(Instruction::CallFunction { dst, func, params });
                },
            }
        }

        match &block.end {
            BasicBlockEnd::Goto { label } => {
                instructions.push(Instruction::Jump { dst: *label });
            }
            BasicBlockEnd::Return { value } => {
                instructions.push(Instruction::Return {
                    value: transform_value(value, registers),
                    max_register
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
                            dst: Register::Temp,
                        });
                        Register::Temp
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
