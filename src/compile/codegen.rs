use std::collections::BTreeMap;

use crate::{
    lexer::Operator,
    ssa::{SsaInstruction, SsaValue, VirtualRegister},
};

use super::{
    Register,
    instruction::{Instruction, StackRegister, Value},
};

pub fn generate_asm(
    ssa: Vec<SsaInstruction>,
    registers: &BTreeMap<VirtualRegister, Register>,
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

    for instr in ssa {
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
                    // Operator::Plus => instructions.push(Instruction::Add {
                    //     reg: Register::Temp,
                    //     value: b,
                    // }),
                    // Operator::Minus => instructions.push(Instruction::Sub {
                    //     reg: Register::Temp,
                    //     value: b,
                    // }),
                    // Operator::Mul => instructions.push(Instruction::Mul {
                    //     reg: Register::Temp,
                    //     value: b,
                    // }),
                    // Operator::Div => instructions.push(Instruction::Div {
                    //     reg: Register::Temp,
                    //     value: b,
                    // }),

                    // Operator::Mod => instructions.push(Instruction::Mod {
                    //     reg: Register::Temp,
                    //     value: b,
                    // }),
                    _ => todo!(),
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
                    crate::lexer::UnaryOperator::Minus => {
                        instructions.push(Instruction::Negate { reg });
                    }
                    _ => todo!(),
                }
            } // SsaInstruction::Return { value } => {
              //     let value = transform_value(value, registers);
              //     instructions.push(Instruction::Return { value });
              // }
        }
    }

    instructions
}

fn transform_value(value: SsaValue, registers: &BTreeMap<VirtualRegister, Register>) -> Value {
    match value {
        SsaValue::Register(virtual_register) => Value::Register(registers[&virtual_register]),
        SsaValue::Immediate(value) => Value::Immediate(value),
    }
}
