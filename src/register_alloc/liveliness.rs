use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use crate::ssa::{
    BasicBlock, BasicBlockEnd, BlockLabel, IrGraph, SsaInstruction, SsaValue, VirtualRegister,
};

pub struct LivelinessContainer<'a> {
    lines: BTreeMap<Line<'a>, BTreeSet<VirtualRegister<'a>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Line<'a> {
    pub block: BlockLabel<'a>,
    pub line: usize,
}

impl<'a> LivelinessContainer<'a> {
    pub fn new(graph: &IrGraph<BasicBlock<'a>>) -> Self {
        let mut this = Self {
            lines: BTreeMap::new(),
        };

        for block in graph.get_return_blocks() {
            this.check_line(
                &Line {
                    block: block.label,
                    line: block.instructions.len(),
                },
                None,
                graph,
            );
        }

        this
    }

    fn check_line(
        &mut self,
        line: &Line<'a>,
        previous_line: Option<&Line<'a>>,
        graph: &IrGraph<BasicBlock<'a>>,
    ) {
        if let Some(previous_line) = previous_line {
            if !self.update_live(line, previous_line) {
                return;
            }
        }

        let block = graph.get(&line.block).unwrap();

        if line.line == block.instructions.len() {
            match &block.end {
                BasicBlockEnd::Return { value } => {
                    self.add_live(line, value);
                }
                BasicBlockEnd::ConditionalJump { condition, .. } => {
                    self.add_live(line, condition);
                }
                BasicBlockEnd::Goto { .. } => (),
            }
        } else {
            match &block.instructions[line.line] {
                SsaInstruction::Move { target, source }
                | SsaInstruction::PhiMove { target, source } => {
                    self.remove_live(line, target);
                    self.add_live(line, source);
                }
                SsaInstruction::BinaryOp {
                    target,
                    a,
                    op: _,
                    b,
                } => {
                    self.remove_live(line, target);
                    self.add_live(line, a);
                    self.add_live(line, b);
                }
                SsaInstruction::UnaryOp {
                    target,
                    op: _,
                    value,
                } => {
                    self.remove_live(line, target);
                    self.add_live(line, &SsaValue::Register(*value));
                }
                SsaInstruction::FunctionArg { index: _, target } => {
                    self.remove_live(line, target);
                }
                SsaInstruction::FunctionCall {
                    target,
                    name: _,
                    args,
                } => {
                    if let Some(target) = target {
                        self.remove_live(line, target);
                    }
                    for arg in args {
                        self.add_live(line, arg);
                    }
                }
                SsaInstruction::Allocate {
                    target,
                    ty: _,
                    array_len,
                } => {
                    if let Some(target) = target {
                        self.remove_live(line, target);
                    }
                    if let Some(array_len) = array_len {
                        self.add_live(line, array_len);
                    }
                }
                SsaInstruction::MemGet {
                    target,
                    source_ptr,
                    offset: _,
                    field_size: _,
                } => {
                    self.remove_live(line, target);
                    self.add_live(line, source_ptr);
                }
                SsaInstruction::MemSet {
                    target_ptr: target,
                    source,
                    offset: _,
                    field_size: _,
                } => {
                    self.add_live(line, &SsaValue::Register(*target));
                    self.add_live(line, source);
                }
                SsaInstruction::CalcArrayPtr {
                    target,
                    ptr,
                    index,
                    struct_size: _,
                } => {
                    self.remove_live(line, target);
                    self.add_live(line, ptr);
                    self.add_live(line, index);
                }
            }
        }

        if line.line == 0 {
            let predecessors = graph.get_predecessors(&line.block);

            for block in predecessors {
                self.check_line(
                    &Line {
                        block: block.label,
                        line: block.instructions.len(),
                    },
                    Some(line),
                    graph,
                );
            }
        } else {
            self.check_line(
                &Line {
                    block: line.block,
                    line: line.line - 1,
                },
                Some(line),
                graph,
            );
        }
    }

    fn update_live(&mut self, line: &Line<'a>, previous_line: &Line<'a>) -> bool {
        let mut did_change = false;

        let previous_set = self.lines.get(previous_line).unwrap().clone();

        if let Some(set) = self.lines.get_mut(line) {
            for value in previous_set {
                if set.insert(value) {
                    did_change = true;
                }
            }
        } else {
            self.lines.insert(*line, previous_set);
            did_change = true;
        }

        return did_change;
    }

    fn add_live(&mut self, line: &Line<'a>, value: &SsaValue<'a>) {
        let set = self.lines.entry(*line).or_insert_with(BTreeSet::new);

        match value {
            SsaValue::Register(virtual_register) => {
                set.insert(*virtual_register);
            }
            SsaValue::ImmediateNum(_) | SsaValue::ImmediateBool(_) => (),
        }
    }

    fn remove_live(&mut self, line: &Line<'a>, value: &VirtualRegister<'a>) {
        let set = self.lines.entry(*line).or_insert_with(BTreeSet::new);
        set.remove(value);
    }

    pub fn get_live_set(&self, line: &Line<'a>) -> &BTreeSet<VirtualRegister<'a>> {
        self.lines.get(line).unwrap()
    }
}

impl<'a> Display for LivelinessContainer<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut current_block = None;

        for (line, live_set) in self.lines.iter() {
            if current_block != Some(line.block) {
                current_block = Some(line.block);
                writeln!(f, "{}:", line.block)?;
            }

            writeln!(
                f,
                "{:2}: {}",
                line.line,
                live_set
                    .iter()
                    .map(|reg| format!("{}", reg))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }

        Ok(())
    }
}

// pub fn analyze_liveliness(instructions: Vec<SsaInstruction>) -> Vec<(SsaInstruction, LiveSet)> {
//     let mut live_set = BTreeSet::new();

//     let instr_with_live = instructions.into_iter().rev().map(|instr| {
//         match instr {
//             SsaInstruction::Move { target, source } => {
//                 live_set.remove(target);
//                 live_set.insert(source);
//             }
//             SsaInstruction::PhiMove { target, source } => {
//                 live_set.remove(target);
//                 live_set.insert(source);
//             }
//             SsaInstruction::BinaryOp {
//                 target,
//                 a,
//                 op: _,
//                 b,
//             } => {
//                 live_set.remove(target);
//                 live_set.insert(a);
//                 live_set.insert(b);
//             }
//             SsaInstruction::UnaryOp {
//                 target,
//                 op: _,
//                 value,
//             } => {
//                 live_set.remove(target);
//                 live_set.insert(SsaValue::Register(value));
//             }
//         }

//         todo!()
//         // (instr, live_set.clone())
//     });

//     let mut instructions = instr_with_live.collect::<Vec<_>>();

//     instructions.reverse();

//     instructions
// }

// #[derive(Debug, Clone)]
// pub struct LiveSet<'a> {
//     live_registers: BTreeSet<VirtualRegister<'a>>,
// }

// impl<'a> LiveSet<'a> {
//     pub fn new() -> Self {
//         Self {
//             live_registers: BTreeSet::new(),
//         }
//     }

//     pub fn insert(&mut self, value: SsaValue<'a>) {
//         match value {
//             SsaValue::Register(virtual_register) => {
//                 self.live_registers.insert(virtual_register);
//             }
//             SsaValue::ImmediateNum(_) => (),
//             SsaValue::ImmediateBool(_) => (),
//         }
//     }

//     pub fn remove(&mut self, register: VirtualRegister<'a>) {
//         self.live_registers.remove(&register);
//     }

//     pub fn registers(&self) -> impl Iterator<Item = &VirtualRegister<'a>> {
//         self.live_registers.iter()
//     }
// }

// impl<'a> Extend<VirtualRegister<'a>> for LiveSet<'a> {
//     fn extend<T: IntoIterator<Item = VirtualRegister<'a>>>(&mut self, iter: T) {
//         self.live_registers.extend(iter);
//     }
// }

// impl<'a> Display for LiveSet<'a> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "[")?;
//         let mut is_first = true;
//         for reg in &self.live_registers {
//             if !is_first {
//                 write!(f, ", ")?;
//             }

//             write!(f, "{}", reg)?;

//             is_first = false;
//         }
//         write!(f, "]")?;

//         Ok(())
//     }
// }
