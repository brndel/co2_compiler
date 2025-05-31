use std::collections::{BTreeMap, BTreeSet};

use crate::{
    lexer::{BinaryOperator, Operator, UnaryOperator},
    ssa::{
        BasicBlock, BasicBlockEnd, BlockLabel, IrGraph, SsaInstruction, SsaValue, VirtualRegister,
    },
};

use super::{
    Register,
    instruction::{Instruction, StackRegister, Value},
};

pub fn generate_asm(
    ir_graph: IrGraph<BasicBlock>,
    registers: &BTreeMap<VirtualRegister, Register>,
    visited_blocks: &BTreeSet<BlockLabel>,
) -> Vec<Instruction> {
    let mut instructions = Vec::new();

    let max_stack_register = registers
        .values()
        .filter_map(|reg| match reg {
            Register::Stack(StackRegister(pos)) => Some(*pos),
            _ => None,
        })
        .max()
        .unwrap_or_default();

    instructions.push(Instruction::AllocateStack {
        bytes: max_stack_register * 4,
    });

    for block in ir_graph {
        if !visited_blocks.contains(&block.label) {
            continue;
        }

        for instr in block.instructions {
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
                        BinaryOperator::LogicAnd => todo!(),
                        BinaryOperator::LogicOr => todo!(),
                        BinaryOperator::BitAnd => todo!(),
                        BinaryOperator::BitOr => todo!(),
                        BinaryOperator::BitXor => todo!(),
                        BinaryOperator::ShiftLeft => todo!(),
                        BinaryOperator::ShiftRight => todo!(),
                        BinaryOperator::Less => todo!(),
                        BinaryOperator::LessEq => todo!(),
                        BinaryOperator::Greater => todo!(),
                        BinaryOperator::GreaterEq => todo!(),
                        BinaryOperator::Equals => todo!(),
                        BinaryOperator::NotEquals => todo!(),
                    }
                    instructions.push(Instruction::Move {
                        src: Register::Temp.into(),
                        dst,
                    });
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
                        UnaryOperator::LogicNot => todo!(),
                        UnaryOperator::BitNot => todo!(),
                    }
                }
            }
        }

        match block.end {
            BasicBlockEnd::Goto { label } => todo!(),
            BasicBlockEnd::Return { value } => {
                instructions.push(Instruction::Return { value: transform_value(value, registers) });
            },
            BasicBlockEnd::ConditionalJump {
                condition,
                on_true,
                on_false,
            } => todo!(),
        }
    }

    instructions
}

fn transform_value(value: SsaValue, registers: &BTreeMap<VirtualRegister, Register>) -> Value {
    match value {
        SsaValue::Register(virtual_register) => Value::Register(registers[&virtual_register]),
        SsaValue::ImmediateNum(value) => Value::Immediate(value),
        SsaValue::ImmediateBool(value) => Value::Immediate(value.then_some(1).unwrap_or(0)),
    }
}
