use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use crate::ssa::{BasicBlock, BasicBlockEnd, BlockLabel, IrGraph, VirtualRegister};

use super::{Line, LivelinessContainer};

#[derive(Debug, Default)]
pub struct LivelinessGraph<'a> {
    vertices: BTreeSet<VirtualRegister<'a>>,
    edges: BTreeMap<VirtualRegister<'a>, BTreeSet<VirtualRegister<'a>>>,
    visited_blocks: BTreeSet<BlockLabel<'a>>,
}

impl<'a> LivelinessGraph<'a> {
    pub fn new(ir_graph: &IrGraph<BasicBlock<'a>>, container: &LivelinessContainer<'a>) -> Self {
        let mut this = Self::default();

        let first_block = ir_graph.get_first().unwrap();

        this.insert_line(
            &Line {
                block: first_block.label,
                line: 0,
            },
            ir_graph,
            container,
        );

        this
    }

    fn insert_line(
        &mut self,
        line: &Line<'a>,
        ir_graph: &IrGraph<BasicBlock<'a>>,
        container: &LivelinessContainer<'a>,
    ) {
        let block = ir_graph.get(&line.block).unwrap();

        if self.visited_blocks.contains(&line.block) {
            return;
        }
        
        if line.line == block.instructions.len() {
            self.visited_blocks.insert(block.label);

            match &block.end {
                BasicBlockEnd::Goto { label } => {
                    self.insert_line(
                        &Line {
                            block: *label,
                            line: 0,
                        },
                        ir_graph,
                        container,
                    );
                }
                BasicBlockEnd::Return { value: _ } => (),
                BasicBlockEnd::ConditionalJump {
                    condition: _,
                    on_true,
                    on_false,
                } => {
                    self.insert_line(
                        &Line {
                            block: *on_true,
                            line: 0,
                        },
                        ir_graph,
                        container,
                    );
                    self.insert_line(
                        &Line {
                            block: *on_false,
                            line: 0,
                        },
                        ir_graph,
                        container,
                    );
                }
            }
        } else {
            let next_line = Line {
                block: line.block,
                line: line.line + 1,
            };

            if let Some(instr) = block.instructions.get(line.line) {
                let live_set = container.get_live_set(&next_line);
                
                if let Some(target) = instr.target() {
                    self.insert_live_set(target, live_set);
                }
            }
            self.insert_line(&next_line, ir_graph, container);
        }
    }

    fn insert_live_set(
        &mut self,
        target: &VirtualRegister<'a>,
        live_set: &BTreeSet<VirtualRegister<'a>>,
    ) {
        self.vertices.insert(*target);

        for register in live_set {
            self.insert_edge(*target, *register);
        }
    }

    fn insert_edge(&mut self, a: VirtualRegister<'a>, b: VirtualRegister<'a>) {
        if a != b {
            self.edges.entry(a).or_default().insert(b);
            self.edges.entry(b).or_default().insert(a);
        }
    }

    pub fn visited_blocks(&self) -> &BTreeSet<BlockLabel<'a>> {
        &self.visited_blocks
    }

    fn max_cardinal_ordering(&self) -> Vec<VirtualRegister> {
        let mut ordered_registers = Vec::new();
        let mut registers = self
            .vertices
            .iter()
            .cloned()
            .map(|key| (key, 0_usize))
            .collect::<BTreeMap<_, _>>();

        while let Some(register) = Self::pop_highest(&mut registers) {
            ordered_registers.push(register);

            if let Some(neighbors) = self.edges.get(&register) {
                for neighbor in neighbors {
                    registers.entry(*neighbor).and_modify(|weight| *weight += 1);
                }
            }
        }

        ordered_registers
    }

    fn pop_highest(
        registers: &mut BTreeMap<VirtualRegister<'a>, usize>,
    ) -> Option<VirtualRegister<'a>> {
        let (&register, _) = registers.iter().max_by_key(|(_, weight)| *weight)?;

        registers.remove(&register);

        Some(register)
    }

    pub fn greedy_coloring<T: GraphColor + Ord>(&'a self) -> BTreeMap<VirtualRegister<'a>, T> {
        let mut color_map = BTreeMap::new();

        let ordered_registers = self.max_cardinal_ordering();

        for register in ordered_registers {
            let neighbors = self.edges.get(&register);

            let neighbor_colors = neighbors
                .iter()
                .flat_map(|neighbors| neighbors.iter())
                .filter_map(|reg| color_map.get(reg))
                .collect::<BTreeSet<_>>();

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
        for reg in &self.vertices {
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
