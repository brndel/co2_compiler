use std::{
    collections::{BTreeMap, BTreeSet},
    mem::swap,
};

use crate::ssa::{
    basic_block::{BasicBlock, BasicBlockEnd, BlockLabel}, counter::Counter, ir_graph::IrGraph, SsaInstruction, VirtualRegister
};

use super::{AssignPhi, ConBasicBlock, PhiAssignment};

pub struct Context<'a> {
    pub counter: Counter<'a>,
    loops: Vec<Loop<'a>>,
    graph: IrGraph<ConBasicBlock<'a>>,
}

impl<'a> Context<'a> {
    pub fn new(func: &'a str) -> Self {
        Self {
            counter: Counter::new(func),
            loops: Vec::default(),
            graph: IrGraph::default(),
        }
    }

    pub fn finish(self) -> IrGraph<BasicBlock<'a>> {
        self.graph.map(|block| {
            assert!(block.is_sealed());

            block.into()
        })
    }

    fn get_var_recursive<End>(
        &mut self,
        var: &'a str,
        label: &BlockLabel<'a>,
        mut block: Option<&mut ConBasicBlock<'a, End>>,
    ) -> VirtualRegister<'a> {
        /// Try to run the code on block
        /// if block is None, get the block from the graph
        macro_rules! mut_block {
            ($id:ident => $body:block) => {
                if let Some($id) = &mut block {
                    $body
                } else {
                    let $id = self.graph.get_mut(label).unwrap();
                    $body
                }
            };
        }

        mut_block!(block => {
            if let Some(reg) = block.get_var(var, &mut self.counter) {
                return reg;
            }
        });

        let predecessors = self.graph.get_predecessors(label).map(|block| block.label).collect::<Vec<_>>();

        if predecessors.len() == 1 {
            let label = predecessors[0];

            return self.get_var_recursive::<BasicBlockEnd>(var, &label, None);
        }

        let phi_register = self.counter.next_register(var);

        mut_block!(block => {
            block.variables.insert(var, phi_register);
        });

        let mut phi_values = BTreeSet::new();

        for label in predecessors {
            let register = self.get_var_recursive::<BasicBlockEnd>(var, &label, None);

            phi_values.insert(register);

            let block = self.graph.get_mut(&label).unwrap();
            block.assigned_phis.push(AssignPhi {
                target: phi_register,
                source: register,
            });
        }

        mut_block!(block => {
            block.phis.push(PhiAssignment {
                target: phi_register,
                phi_sources: phi_values,
            });
        });

        return phi_register;
    }

    pub fn seal_block(&mut self, label: &BlockLabel<'a>) {
        let block = self.graph.get_mut(&label).unwrap();

        let Some(vars) = block.unfinished_variables.take() else {
            return;
        };

        for (var, phi_register) in vars {
            let predecessors = self.graph.get_predecessors(&label).map(|block| block.label).collect::<Vec<_>>();

            let mut phi_values = BTreeSet::new();

            for label in predecessors {
                let register = self.get_var_recursive::<BasicBlockEnd>(var, &label, None);

                phi_values.insert(register);

                let block = self.graph.get_mut(&label).unwrap();
                block.assigned_phis.push(AssignPhi {
                    target: phi_register,
                    source: register,
                });
            }

            let block = self.graph.get_mut(&label).unwrap();
            block.phis.push(PhiAssignment {
                target: phi_register,
                phi_sources: phi_values,
            });
        }
    }

    pub fn get_loop(&self) -> &Loop<'a> {
        self.loops.last().unwrap()
    }

    pub fn push_loop(&mut self, r#loop: Loop<'a>) {
        self.loops.push(r#loop);
    }

    pub fn pop_loop(&mut self) {
        self.loops.pop();
    }
}

pub struct Loop<'a> {
    pub next: BlockLabel<'a>,
    pub end: BlockLabel<'a>,
}

pub struct BlockBuilder<'a> {
    next_label: BlockLabel<'a>,
    end_label: BlockLabel<'a>,

    block: ConBasicBlock<'a, ()>,
    is_closed: bool,
}

impl<'a> BlockBuilder<'a> {
    pub fn new(start_label: BlockLabel<'a>, end_label: BlockLabel<'a>, ctx: &mut Context<'a>) -> Self {
        Self {
            end_label,
            next_label: ctx.counter.next_block_label(None),
            block: ConBasicBlock::new(start_label),
            is_closed: false,
        }
    }

    pub fn unsealed(mut self) -> Self {
        self.block.unfinished_variables = Some(BTreeMap::new());

        self
    }
}

impl<'a> BlockBuilder<'a> {
    pub fn push_instruction(&mut self, instruction: SsaInstruction<'a>) {
        self.block.instructions.push(instruction);
    }

    pub fn set_variable(&mut self, var: &'a str, register: VirtualRegister<'a>) {
        self.block.variables.insert(var, register);
    }

    pub fn get_variable(&mut self, var: &'a str, ctx: &mut Context<'a>) -> VirtualRegister<'a> {
        let label = self.block.label;
        ctx.get_var_recursive(var, &label, Some(&mut self.block))
    }

    pub fn next_label(&self) -> BlockLabel<'a> {
        self.next_label
    }
}

impl<'a> BlockBuilder<'a> {
    pub fn end(&mut self, end: BasicBlockEnd<'a>, ctx: &mut Context<'a>) {
        let next_label = self.next_label;
        self.next_label = ctx.counter.next_block_label(None);

        let mut block = ConBasicBlock::new(next_label);
        swap(&mut block, &mut self.block);

        let block = block.with_end(end);

        ctx.graph.insert(block);
    }

    pub fn close(&mut self, ctx: &mut Context<'a>) {
        self.close_with(
            BasicBlockEnd::Goto {
                label: self.end_label,
            },
            ctx,
        );
    }

    pub fn close_with(&mut self, end: BasicBlockEnd<'a>, ctx: &mut Context<'a>) {
        if !self.is_closed {
            self.is_closed = true;

            self.end(end, ctx);
        }
    }

    pub fn is_closed(&self) -> bool {
        self.is_closed
    }
}
