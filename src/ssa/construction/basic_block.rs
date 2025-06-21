use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use crate::ssa::{
    SsaInstruction, SsaValue, VirtualRegister,
    basic_block::{BasicBlock, BasicBlockEnd, BlockLabel},
    counter::Counter,
    ir_graph::IrGraphNode,
};

/// # Construction Basic Block
/// A Basic block with extra information needed for basic block construction
pub struct ConBasicBlock<'a, End = BasicBlockEnd<'a>> {
    pub label: BlockLabel<'a>,
    pub phis: Vec<PhiAssignment<'a>>,
    pub instructions: Vec<SsaInstruction<'a>>,
    pub assigned_phis: Vec<AssignPhi<'a>>,
    pub variables: BTreeMap<&'a str, VirtualRegister<'a>>,
    pub unfinished_variables: Option<BTreeMap<&'a str, VirtualRegister<'a>>>,
    pub end: End,
}

impl<'a, End> ConBasicBlock<'a, End> {
    pub fn is_sealed(&self) -> bool {
        self.unfinished_variables.is_none()
    }
}

impl<'a, End: Display> Display for ConBasicBlock<'a, End> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.label)?;
        for phi in &self.phis {
            writeln!(f, "{}", phi)?;
        }

        for instr in &self.instructions {
            writeln!(f, "{}", instr)?;
        }

        for assign in &self.assigned_phis {
            writeln!(f, "{}", assign)?;
        }

        writeln!(f, "{}", self.end)?;

        Ok(())
    }
}

impl<'a> IrGraphNode for ConBasicBlock<'a> {
    type Id = BlockLabel<'a>;

    fn id(&self) -> Self::Id {
        self.label
    }

    fn is_predecessor(&self, id: &Self::Id) -> bool {
        self.end.goes_to(&id)
    }
}

impl<'a> From<ConBasicBlock<'a>> for BasicBlock<'a> {
    fn from(value: ConBasicBlock<'a>) -> Self {
        let mut instructions = value.instructions;
        instructions.extend(
            value
                .assigned_phis
                .into_iter()
                .map(|phi| SsaInstruction::PhiMove {
                    target: phi.target,
                    source: SsaValue::Register(phi.source),
                }),
        );

        BasicBlock {
            label: value.label,
            instructions,
            end: value.end,
        }
    }
}

pub struct PhiAssignment<'a> {
    pub target: VirtualRegister<'a>,
    pub phi_sources: BTreeSet<VirtualRegister<'a>>,
}

impl<'a> Display for PhiAssignment<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} = phi({})",
            self.target,
            self.phi_sources
                .iter()
                .map(|reg| reg.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

pub struct AssignPhi<'a> {
    pub target: VirtualRegister<'a>,
    pub source: VirtualRegister<'a>,
}

impl<'a> Display for AssignPhi<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "phi({}) = {}", self.target, self.source)
    }
}

impl<'a, End> ConBasicBlock<'a, End> {
    pub fn new(label: BlockLabel<'a>) -> Self
    where
        End: Default,
    {
        Self {
            label,
            phis: Vec::new(),
            instructions: Vec::new(),
            assigned_phis: Vec::new(),
            variables: BTreeMap::new(),
            unfinished_variables: None,
            end: End::default(),
        }
    }

    pub fn get_var(&mut self, var: &'a str, counter: &mut Counter) -> Option<VirtualRegister<'a>> {
        if let Some(reg) = self.variables.get(var) {
            return Some(*reg);
        }

        if let Some(unfinished_variables) = &mut self.unfinished_variables {
            let phi_register = counter.next_register(var);

            self.variables.insert(var, phi_register);
            unfinished_variables.insert(var, phi_register);

            return Some(phi_register);
        }

        return None;
    }

    pub fn with_end<T>(self, end: T) -> ConBasicBlock<'a, T> {
        ConBasicBlock {
            label: self.label,
            phis: self.phis,
            instructions: self.instructions,
            assigned_phis: self.assigned_phis,
            variables: self.variables,
            unfinished_variables: self.unfinished_variables,
            end,
        }
    }
}
