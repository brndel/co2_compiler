use std::{collections::BTreeSet, fmt::Display};

use crate::ssa::{SsaInstruction, SsaValue, VirtualRegister};

pub fn analyze_liveliness(instructions: Vec<SsaInstruction>) -> Vec<(SsaInstruction, LiveSet)> {
    let mut live_set = LiveSet::new();

    let instr_with_live = instructions.into_iter().rev().map(|instr| {
        match instr {
            SsaInstruction::Move { target, source } => {
                live_set.remove(target);
                live_set.insert(source);
            }
            SsaInstruction::PhiMove { target, source } => {
                live_set.remove(target);
                live_set.insert(source);
            }
            SsaInstruction::BinaryOp {
                target,
                a,
                op: _,
                b,
            } => {
                live_set.remove(target);
                live_set.insert(a);
                live_set.insert(b);
            }
            SsaInstruction::UnaryOp {
                target,
                op: _,
                value,
            } => {
                live_set.remove(target);
                live_set.insert(SsaValue::Register(value));
            }
        }

        todo!()
        // (instr, live_set.clone())
    });

    let mut instructions = instr_with_live.collect::<Vec<_>>();

    instructions.reverse();

    instructions
}

#[derive(Debug, Clone)]
pub struct LiveSet<'a> {
    live_registers: BTreeSet<VirtualRegister<'a>>,
}

impl<'a> LiveSet<'a> {
    pub fn new() -> Self {
        Self {
            live_registers: BTreeSet::new(),
        }
    }

    pub fn insert(&mut self, value: SsaValue<'a>) {
        todo!()
        // match value {
        //     SsaValue::Register(virtual_register) => {
        //         self.live_registers.insert(virtual_register);
        //     }
        //     SsaValue::Immediate(_) => (),
        // }
    }

    pub fn remove(&mut self, register: VirtualRegister<'a>) {
        self.live_registers.remove(&register);
    }

    pub fn registers(&self) -> impl Iterator<Item = &VirtualRegister<'a>> {
        self.live_registers.iter()
    }
}

impl<'a> Display for LiveSet<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut is_first = true;
        for reg in &self.live_registers {
            if !is_first {
                write!(f, ", ")?;
            }

            write!(f, "{}", reg)?;

            is_first = false;
        }
        write!(f, "]")?;

        Ok(())
    }
}
