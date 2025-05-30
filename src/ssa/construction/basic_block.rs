use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use crate::ssa::{
    basic_block::{BasicBlock, BasicBlockEnd, BlockLabel}, SsaInstruction, SsaValue, VirtualRegister
};

/// # Construction Basic Block
/// A Basic block with extra information needed for basic block construction
pub struct ConBasicBlock<'a> {
    pub label: BlockLabel,
    pub phis: Vec<PhiAssignment<'a>>,
    pub instructions: Vec<SsaInstruction<'a>>,
    pub assigned_phis: Vec<AssignPhi<'a>>,
    pub end: BasicBlockEnd<'a>,
    pub variables: BTreeMap<&'a str, VirtualRegister<'a>>,
    pub unfinished_variables: Option<BTreeMap<&'a str, VirtualRegister<'a>>>,
}

impl<'a> ConBasicBlock<'a> {
    pub fn is_sealed(&self) -> bool {
        self.unfinished_variables.is_none()
    }
}

impl<'a> Display for ConBasicBlock<'a> {
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
