use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use crate::ssa::{SsaInstruction, VirtualRegister};

use super::LiveSet;

#[derive(Debug, Default)]
pub struct IrGraph<'a> {
    vertices: BTreeMap<VirtualRegister<'a>, SsaInstruction<'a>>,
    edges: BTreeMap<VirtualRegister<'a>, BTreeSet<VirtualRegister<'a>>>,
}

impl<'a> IrGraph<'a> {
    pub fn new(instructions: Vec<(SsaInstruction<'a>, LiveSet<'a>)>) -> Self {
        let mut this = Self::default();

        for instrs in instructions.windows(2) {
            let (current_instr, _) = &instrs[0];
            let (_, live_set) = &instrs[1];

            let Some(reg) = current_instr.target() else {
                continue;
            };

            this.vertices.insert(*reg, *current_instr);

            for live_reg in live_set.registers() {
                this.insert_edge(*reg, *live_reg);
            }
        }

        this
    }

    fn insert_edge(&mut self, a: VirtualRegister<'a>, b: VirtualRegister<'a>) {
        if a != b {
            self.edges.entry(a).or_default().insert(b);
            self.edges.entry(b).or_default().insert(a);
        }
    }
}

impl<'a> Display for IrGraph<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (reg, instr) in &self.vertices {
            write!(f, "{} -- [", reg)?;

            if let Some(connected) = self.edges.get(&reg) {
                let mut is_first = true;
                for reg in connected {
                    if !is_first {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", reg)?;

                    is_first = false;
                }
            }

            writeln!(f, "]")?;
        }

        Ok(())
    }
}
