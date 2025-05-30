use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use crate::ssa::{SsaInstruction, VirtualRegister};

use super::LiveSet;

#[derive(Debug, Default)]
pub struct LivelinessGraph<'a> {
    vertices: BTreeMap<VirtualRegister<'a>, SsaInstruction<'a>>,
    edges: BTreeMap<VirtualRegister<'a>, BTreeSet<VirtualRegister<'a>>>,
}

impl<'a> LivelinessGraph<'a> {
    pub fn new(instructions: Vec<(SsaInstruction<'a>, LiveSet<'a>)>) -> Self {
        let mut this = Self::default();

        for instrs in instructions.windows(2) {
            let (current_instr, _) = &instrs[0];
            let (_, live_set) = &instrs[1];

            todo!();
            // let Some(reg) = current_instr.target() else {
            //     continue;
            // };

            // this.vertices.insert(*reg, *current_instr);

            // for live_reg in live_set.registers() {
            //     this.insert_edge(*reg, *live_reg);
            // }
        }

        this
    }

    fn insert_edge(&mut self, a: VirtualRegister<'a>, b: VirtualRegister<'a>) {
        if a != b {
            self.edges.entry(a).or_default().insert(b);
            self.edges.entry(b).or_default().insert(a);
        }
    }

    fn max_cardinal_ordering(&self) -> Vec<VirtualRegister> {
        
        let mut ordered_registers = Vec::new();
        let mut registers = self.vertices.keys().cloned().map(|key| (key, 0_usize)).collect::<BTreeMap<_, _>>();

        while let Some(register) =  Self::pop_highest(&mut registers) {
            ordered_registers.push(register);

            if let Some(neighbors) = self.edges.get(&register) {
                for neighbor in neighbors {
                    registers.entry(*neighbor).and_modify(|weight| *weight += 1);
                }
            }

        }

        ordered_registers
    }

    fn pop_highest(registers: &mut BTreeMap<VirtualRegister<'a>, usize>) -> Option<VirtualRegister<'a>> {
            let (&register, _) = registers.iter().max_by_key(|(_, weight)| *weight)?;

            registers.remove(&register);

            Some(register)
    }

    pub fn greedy_coloring<T: GraphColor + Ord>(&'a self) -> BTreeMap<VirtualRegister<'a>, T> {
        let mut color_map = BTreeMap::new();

        let ordered_registers = self.max_cardinal_ordering();

        for register in ordered_registers {
            
            let neighbors = self.edges.get(&register);

            let neighbor_colors = neighbors.iter().flat_map(|neighbors| neighbors.iter()).filter_map(|reg| color_map.get(reg)).collect::<BTreeSet<_>>();

            for color in T::ascending_iter() {
                if !neighbor_colors.contains(&color) {
                    color_map.insert(register, color);
                    break;
                }
            }
        }


        color_map
    }
}

impl<'a> Display for LivelinessGraph<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (reg, _) in &self.vertices {
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

pub trait GraphColor {
    fn ascending_iter() -> impl Iterator<Item = Self>;
}