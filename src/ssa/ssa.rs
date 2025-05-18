use std::collections::BTreeMap;

use crate::parser::{Expression, Statement};

use super::{
    instruction::{SsaInstruction, SsaValue},
    register::{RegisterCounter, VirtualRegister},
};

pub fn to_ssa<'a>(statements: Vec<Statement<'a>>) -> Vec<SsaInstruction<'a>> {
    let mut variables = BTreeMap::new();
    let mut reg_counter = RegisterCounter::new();
    let mut instructions = Vec::new();

    for statement in statements {
        match statement {
            Statement::Declaration { ident, value } => {
                if let Some(value) = value {
                    let value = expr_to_ssa(&mut instructions, &variables, &mut reg_counter, value);

                    let target = reg_counter.next_label(ident);

                    instructions.push(SsaInstruction::Move { target, source: value });
                    variables.insert(ident.0, target);
                }
            }
            Statement::Assignment { ident, op, value } => {
                let value = expr_to_ssa(&mut instructions, &variables, &mut reg_counter, value);

                let target = reg_counter.next_label(ident);

                match op {
                    Some(op) => {
                        let current_value = variables[ident.0];

                        instructions.push(SsaInstruction::BinaryOp {
                            target,
                            a: SsaValue::Register(current_value),
                            op,
                            b: value,
                        });
                    }
                    None => {
                        instructions.push(SsaInstruction::Move {
                            target,
                            source: value,
                        });
                    }
                }

                variables.insert(ident.0, target);
            }
            Statement::Return { expr } => {
                let value = expr_to_ssa(&mut instructions, &variables, &mut reg_counter, expr);

                instructions.push(SsaInstruction::Return { value });
            },
        }
    }

    instructions
}

pub fn expr_to_ssa<'a>(
    instructions: &mut Vec<SsaInstruction<'a>>,
    variables: &BTreeMap<&str, VirtualRegister<'a>>,
    reg_counter: &mut RegisterCounter,
    expr: Expression<'a>,
) -> SsaValue<'a> {
    match expr {
        Expression::Ident(ident) => SsaValue::Register(variables[ident.0]),
        Expression::Num(num) => SsaValue::Immediate(num.num()),
        Expression::Binary { a, op, b } => {
            let a = expr_to_ssa(instructions, variables, reg_counter, *a);
            let b = expr_to_ssa(instructions, variables, reg_counter, *b);

            let target = reg_counter.next();

            instructions.push(SsaInstruction::BinaryOp { target, a, op, b });

            SsaValue::Register(target)
        }
        Expression::Unary { op, expr } => {
            let value = expr_to_ssa(instructions, variables, reg_counter, *expr);
            
            let value = match value {
                SsaValue::Register(virtual_register) => virtual_register,
                SsaValue::Immediate(value) => {
                    let temp_reg = reg_counter.next();
                    instructions.push(SsaInstruction::Move { target: temp_reg, source: SsaValue::Immediate(value) });

                    temp_reg
                },
            };

            let target = reg_counter.next();
            instructions.push(SsaInstruction::UnaryOp { target, op, value });

            SsaValue::Register(target)
        }
    }
}
